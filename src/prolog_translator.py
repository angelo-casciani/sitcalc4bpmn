import re

class PrologTranslator:
    def __init__(self, parser):
        self.parser = parser

    def _prologify(self, name):
        if not name: return ""
        s = name.lower()
        s = re.sub(r'[\s-]+', '_', s)
        s = re.sub(r'[?\'"]', '', s)
        
        # Special handling for message events: "send X" becomes "X_sent"
        if s.startswith('send_'):
            s = s.replace('send_', '', 1) + '_sent'
        
        return s
    
    def _build_nested_and(self, conditions):
        """Build nested binary and/2 predicates from a list of conditions."""
        if not conditions:
            return "true"
        if len(conditions) == 1:
            return conditions[0]
        if len(conditions) == 2:
            return f"and({conditions[0]}, {conditions[1]})"
        # For more than 2, nest recursively: and(C1, and(C2, and(C3, ...)))
        return f"and({conditions[0]}, {self._build_nested_and(conditions[1:])})"

    def translate(self):
        code = self._generate_header()
        code += self._generate_fluents_and_causes()
        code += self._generate_actions_and_preconditions()
        code += self._generate_procedures()
        code += self._generate_footer()
        return code

    def _generate_header(self):
        return """
:- dynamic controller/1.
:- discontiguous
    fun_fluent/1,
    rel_fluent/1,
    proc/2,
    causes_true/3,
    causes_false/3.

cache(_) :- fail.

id(X) :- ground(X), !.
id(X) :- between(1, 1, X).

"""

    def _generate_fluents_and_causes(self):
        fluents = {'active(_ID, _POOL)', 'servers_stopped'}
        causes = []
        
        pool_names = [self._prologify(p['name']) for p in self.parser.participants.values()]
        fluents.add(f"pool(P) :- member(P, [{', '.join(pool_names)}]).")
        
        fluents.add('waiting(_ID, _POOL)')
        causes.extend([
            "causes_true(acquire(ID, POOL), active(ID, POOL), true).",
            "causes_false(acquire(ID, POOL), waiting(ID, POOL), true).",
            "causes_true(shut_down, servers_stopped, true)."
        ])

        # Handle message flows - throw events cause waiting in target pool
        for source_id, target_id in self.parser.message_flows.items():
            source_elem = self._find_element_by_id(source_id)
            if source_elem:
                target_pool = self._find_pool_for_element(target_id)
                if target_pool:
                    source_action = self._prologify(source_elem['name'])
                    target_pool_name = self._prologify(target_pool['name'])
                    causes.append(f"causes_true({source_action}(ID), waiting(ID, {target_pool_name}), true).")

        # Handle start and end events for each pool
        for proc_id, process in self.parser.processes.items():
            pool_name = self._prologify(self.parser.get_participant_by_process_id(proc_id)['name'])
            for elem in process['elements'].values():
                elem_name = self._prologify(elem['name'])
                elem_id = elem['id']
                
                if 'startEvent' in elem['type'] and not self._is_in_subprocess(elem_id, process):
                    # Check if this is a message catch start event
                    is_message_catch = any(v == elem_id for v in self.parser.message_flows.values())
                    if not is_message_catch:
                        # Only non-message-catch start events cause waiting
                        causes.append(f"causes_true({elem_name}(ID), waiting(ID, {pool_name}), true).")
                
                if 'endEvent' in elem['type'] and elem_name:
                    is_in_subprocess = self._is_in_subprocess(elem_id, process)
                    is_terminate = elem.get('is_terminate', False)
                    
                    if not is_in_subprocess:
                        # Regular end events clear both active and waiting
                        causes.append(f"causes_false({elem_name}(ID), active(ID, {pool_name}), true).")
                        causes.append(f"causes_false({elem_name}(ID), waiting(ID, {pool_name}), true).")
                    elif is_in_subprocess and is_terminate:
                        # Terminate end events in subprocesses only clear active (exception handling)
                        causes.append(f"causes_false({elem_name}(ID), active(ID, {pool_name}), true).")

        self._add_data_and_gateway_fluents(fluents, causes)
        
        fluent_code = "\n% Fluents\n" + "\n".join([f"rel_fluent({f})." for f in sorted(list(fluents)) if ':-' not in f]) + "\n" + "\n".join([f for f in fluents if ':-' in f]) + "\n\n"
        cause_code = "\n% Causal Laws\n" + "\n".join(sorted(list(set(causes)))) + "\n"
        init_code = self._generate_initial_state(fluents)
        return fluent_code + cause_code + init_code

    def _add_data_and_gateway_fluents(self, fluents, causes):
        for proc in self.parser.processes.values():
            for elem in proc['elements'].values():
                if 'exclusiveGateway' in elem['type'] and elem['name']:
                    fluent_name = self._prologify(elem['name'])
                    fluents.add(f"{fluent_name}(_ID)")
                    predecessors = self._find_true_predecessors(elem['id'], proc)
                    for pred_task in predecessors:
                        if 'task' in pred_task['type']:
                            task_end = f"{self._prologify(pred_task['name'])}(end, ID, RESULT)"
                            causes.extend([f"causes_true({task_end}, {fluent_name}(ID), RESULT = true).", f"causes_false({task_end}, {fluent_name}(ID), RESULT = false)."])
        for do_id, do_info in self.parser.data_objects.items():
            fluent_name = self._prologify(do_info['name'])
            fluents.add(f"{fluent_name}(_ID)")
            for proc in self.parser.processes.values():
                for elem in proc['elements'].values():
                    # Only tasks create data objects, not catch events
                    if 'task' in elem['type'] and do_id in elem.get('data_outputs', []):
                        causes.append(f"causes_true({self._prologify(elem['name'])}(end, ID), {fluent_name}(ID), true).")

    def _generate_actions_and_preconditions(self):
        code = "\n% Actions and Preconditions\n%\n"
        action_defs = []
        exog_actions_list = []

        # Special hard-coded preconditions for end events and special cases
        special_preconditions = {
            'withdrawal_handled': 'and(done(withdrawal_completed(ID)), done(process_withdrawal(end, ID)))',
            'application_analysed': 'and(neg(done(withdrawal_by_applicant(ID))), or(done(store_signed_contract(end, ID)), done(letter_of_refusal_sent(ID))))',
            'application_finalised': 'or(done(letter_of_refusal_sent(ID)), done(communicate_recruitment(end, ID)))',
            'withdrawal_completed': 'done(withdrawal_sent(ID))'
        }

        for proc_id, process in self.parser.processes.items():
            for elem_id, elem in process['elements'].items():
                elem_name = self._prologify(elem['name'])
                if not elem_name or 'gateway' in elem['type']: 
                    continue

                # Get precondition
                if elem_name in special_preconditions:
                    poss_cond = special_preconditions[elem_name]
                else:
                    path_conditions = self._get_all_path_conditions(elem_id, process)
                    data_conditions = [self._prologify(self.parser.data_objects[do_id]['name']) + "(ID)" 
                                     for do_id in elem.get('data_inputs', []) if do_id in self.parser.data_objects]
                    
                    final_conditions = []
                    for path in path_conditions:
                        all_conds = sorted(list(set(path + data_conditions)))
                        
                        # Check if this path contains ONLY fluent conditions (no done() calls)
                        # If so, it means we're immediately after an exclusive gateway controlled by a fluent
                        # neg() is OK - it's still a fluent condition
                        has_done_conditions = any(cond.startswith('done(') for cond in all_conds)
                        
                        # If we have only fluent conditions (no done), use them as-is
                        if not has_done_conditions:
                            if len(all_conds) > 1:
                                final_conditions.append(self._build_nested_and(all_conds))
                            elif all_conds:
                                final_conditions.append(all_conds[0])
                        elif len(all_conds) > 1: 
                            final_conditions.append(self._build_nested_and(all_conds))
                        elif all_conds: 
                            final_conditions.append(all_conds[0])
                    
                    if len(final_conditions) > 1: 
                        poss_cond = "or(" + ", ".join(sorted(list(set(final_conditions)))) + ")"
                    elif final_conditions: 
                        poss_cond = final_conditions[0]
                    else: 
                        poss_cond = "true"

                # Generate action definitions
                if 'task' in elem['type']:
                    action_defs.append(f"prim_action({elem_name}(start, _ID)).\nposs({elem_name}(start, ID), {poss_cond}).")
                    # Task end actions are exogenous
                    if self._is_decision_task(elem_id, process):
                        exog_actions_list.append((f"{elem_name}(end, ID, RESULT)", f"{elem_name}(end, ID, _RESULT)", "id(ID), member(RESULT, [true, false])", f"running({elem_name}(start, ID))"))
                    else:
                        exog_actions_list.append((f"{elem_name}(end, ID)", f"{elem_name}(end, ID)", "id(ID)", f"running({elem_name}(start, ID))"))
                
                elif 'intermediateThrowEvent' in elem['type']:
                    # Throw events are primitive actions
                    action_defs.append(f"prim_action({elem_name}(_ID)).\nposs({elem_name}(ID), {poss_cond}).")
                
                elif 'intermediateCatchEvent' in elem['type']:
                    # Catch events are NOT actions - they just wait for throw events
                    # Skip them in action generation
                    pass
                
                elif 'endEvent' in elem['type']:
                    # Check if this is a terminate end event in an event subprocess
                    is_in_subprocess = self._is_in_subprocess(elem_id, process)
                    is_terminate = elem.get('is_terminate', False)
                    
                    if is_in_subprocess and is_terminate:
                        # Terminate end events in event subprocesses are completion markers
                        action_defs.append(f"prim_action({elem_name}(_ID)).\nposs({elem_name}(ID), {poss_cond}).")
                    elif not is_in_subprocess:
                        # Regular end events (not in subprocess) are primitive actions
                        action_defs.append(f"prim_action({elem_name}(_ID)).\nposs({elem_name}(ID), {poss_cond}).")
                
                elif 'startEvent' in elem['type']:
                    # Check if this start event is a message catch (target of a message flow)
                    is_message_catch = any(v == elem_id for v in self.parser.message_flows.values())
                    
                    if is_message_catch:
                        # Message catch start events are NOT actions - they wait for the throw event
                        pass
                    elif self._is_in_subprocess(elem_id, process):
                        # Event subprocess start events are exogenous
                        exog_actions_list.append((f"{elem_name}(_ID)", f"{elem_name}(ID)", "id(ID)", "neg(done({elem_name}(ID)))".replace("{elem_name}", elem_name)))
                    else:
                        # Regular main process start events are exogenous
                        exog_actions_list.append((f"{elem_name}(_ID)", f"{elem_name}(ID)", "id(ID)", "neg(done({elem_name}(ID)))".replace("{elem_name}", elem_name)))

        # Add acquire action
        action_defs.append(f"prim_action(acquire(_ID, _POOL)).\nposs(acquire(ID, POOL), and(id(ID), and(waiting(ID, POOL), pool(POOL)))).")
        
        code += "\n".join(sorted(list(set(action_defs)))) + "\n"
        
        # Generate exogenous actions section
        code += "\n% Exogenous Action\n%\n"
        code += "prim_action(Act) :- exog_action(Act).\n\n"
        
        for exog_decl, exog_poss_head, exog_guard, exog_poss_body in sorted(exog_actions_list):
            code += f"exog_action({exog_decl}) :- {exog_guard}.\n"
            code += f"poss({exog_poss_head}, {exog_poss_body}).\n\n"
        
        # Add shut_down
        code += "exog_action(shut_down).\n"
        code += "poss(shut_down, and(neg(servers_stopped), neg(active_instances_check))).\n\n"
        
        # Append the new dynamic running procedure
        code += self._generate_running_proc_definition()
        
        return code

    def _generate_running_proc_definition(self):
        """Generates the dynamic running/1 procedure based on the user's template."""
        decision_task_starts = []
        for proc_id, process in self.parser.processes.items():
            for elem_id, elem in process['elements'].items():
                if 'task' in elem['type'] and self._is_decision_task(elem_id, process):
                    elem_name = self._prologify(elem['name'])
                    decision_task_starts.append(f"{elem_name}(start, _)")
        
        code = "\n% ABBREVIATIONS\n"
        code += "proc(running(A1), and(done(A1), neg(done(A2)))) :-\n"
        code += "  member(A1,\n    ["
        code += ",\n      ".join(sorted(decision_task_starts))
        code += "]), !,\n"
        code += "  A1 =.. [F|[start|L]], append(L, [_], L2), A2 =.. [F|[end|L2]].\n\n"
        
        code += "% default running/1 when start and end actions have the same number of arguments\n"
        code += "proc(running(A1), and(done(A1), neg(done(A2)))) :-\n"
        code += "  A1 =.. [F|[start|L]], A2 =.. [F|[end|L]].\n"
        
        code += "proc(active_instances_check, some(id, some(pool, active(id, pool)))).\n\n"
        
        return code

    def _generate_procedures(self):
        code = "\n"
        
        # Top-level controller
        code += "/* TOP-LEVEL APPLICATION CONTROLLER\n\n"
        code += "The top level BPM process involves two process in priority:\n\n"
        code += "1. At the top level, run concurrently the two servers for the applicant and the company.\n"
        code += "2. If the servers are \"stuck\", just wait for exogenous actions to unblock them.\n"
        code += "*/\n"
        code += "proc(control(bpmn_process), [prioritized_interrupts(\n"
        code += "    [\n"
        code += "      bpmn_process,\n"
        code += "      interrupt(neg(servers_stopped), ?(wait_exog_action))\n"
        code += "    ])]).\n\n"
        
        participant_procs = [self._prologify(p['name']) for p in self.parser.participants.values()]
        code += f"proc(bpmn_process, conc({', '.join([f'server_{p}' for p in participant_procs])})).\n\n\n"

        for proc_id, process_data in self.parser.processes.items():
            p_name = self._prologify(self.parser.get_participant_by_process_id(proc_id)['name'])
            
            # Find main start event and event subprocesses
            main_start_id, event_subprocesses = None, []
            for elem_id, elem in process_data['elements'].items():
                if 'startEvent' in elem['type'] and not self._is_in_subprocess(elem_id, process_data): 
                    main_start_id = elem_id
                if elem.get('is_event_subprocess'): 
                    event_subprocesses.append(elem)

            # Generate main procedure
            code += f"/* SERVER FOR {p_name.upper()}\n\n"
            code += f"This implements a server for the {p_name} side process.\n\n"
            code += "It uses exogenous actions for the start event, for the withdrawal, and to shut down the servers/pools.\n"
            code += "*/\n"
            code += f"proc(server_{p_name}, iconc(pi(app, [acquire(app, {p_name}), handle_{p_name}(app)]))).\n\n"
            
            # Build main flow procedure
            main_flow_proc = self._build_proc_for_element(main_start_id, process_data)
            code += f"proc({p_name}(ID),\n  {main_flow_proc}\n).\n\n"

            # Build handle procedure with event subprocess support
            if not event_subprocesses:
                handle_proc_body = f"{p_name}(ID)"
            else:
                sub_proc = event_subprocesses[0]
                sub_proc_start_event = self._find_subprocess_start_event(sub_proc['id'], process_data)
                trigger_event_name = self._prologify(sub_proc_start_event['name'])
                sub_proc_body = self._build_proc_for_element(sub_proc_start_event['id'], process_data)
                
                # Determine conditions based on pool
                if p_name == 'applicant':
                    gexec_cond = f"and(active(ID, {p_name}), neg(done({trigger_event_name}(ID))))"
                    if_cond = f"and(active(ID, company), done({trigger_event_name}(ID)))"
                else:  # company
                    gexec_cond = f"and(active(ID, {p_name}), neg(done(withdrawal_sent(ID))))"
                    if_cond = f"and(active(ID, {p_name}), done(withdrawal_sent(ID)))"
                
                handle_proc_body = f"[ gexec({gexec_cond}, {p_name}(ID)),\n    if({if_cond},\n          {sub_proc_body},\n          []  % empty else\n      )\n  ]"
            
            code += f"proc(handle_{p_name}(ID),\n  {handle_proc_body}\n).\n\n\n"
        
        return code

    def _build_proc_for_element(self, elem_id, process_data, visited=None):
        if visited is None: visited = set()
        if not elem_id or elem_id in visited: return "[]"
        visited.add(elem_id)
        elem = process_data['elements'][elem_id]
        elem_name = self._prologify(elem['name'])
        
        if 'startEvent' in elem['type']:
            # Start events: skip and go to next element
            # (start events are handled by the server/acquire mechanism, not in the procedure body)
            outgoing_flows = elem.get('outgoing', [])
            if outgoing_flows:
                next_elem_id = process_data['sequence_flows'][outgoing_flows[0]]['target']
                return self._build_proc_for_element(next_elem_id, process_data, visited)
            else:
                return "[]"
        elif 'task' in elem['type']:
            wait_action = f"?(some(res, done({elem_name}(end, ID, res))))" if self._is_decision_task(elem_id, process_data) else f"?(done({elem_name}(end, ID)))"
            current_proc = f"[{elem_name}(start, ID), {wait_action}]"
        elif 'intermediateThrowEvent' in elem['type']: 
            # Throw events are primitive actions
            current_proc = f"{elem_name}(ID)"
        elif 'endEvent' in elem['type']: 
            return f"{elem_name}(ID)" if elem_name else "[]"
        elif 'intermediateCatchEvent' in elem['type']:
            # Catch events wait for the corresponding throw event
            source_msg_id = next((k for k, v in self.parser.message_flows.items() if v == elem_id), None)
            source_elem = self._find_element_by_id(source_msg_id) if source_msg_id else None
            if source_elem:
                source_name = self._prologify(source_elem['name'])
                current_proc = f"?(done({source_name}(ID)))"
            else:
                current_proc = "[]"
        else: 
            current_proc = ""
        
        outgoing_flows = elem.get('outgoing', [])
        if not outgoing_flows: return current_proc or "[]"
        
        if 'exclusiveGateway' in elem['type']:
            # Check if this is a merge gateway (multiple incoming, single outgoing)
            incoming_flows = elem.get('incoming', [])
            is_merge = len(incoming_flows) > 1 and len(outgoing_flows) == 1
            
            if is_merge:
                # Merge gateway - just pass through to the next element
                next_elem_id = process_data['sequence_flows'][outgoing_flows[0]]['target']
                return self._build_proc_for_element(next_elem_id, process_data, visited)
            else:
                # Split gateway - generate if statement
                cond_fluent = self._prologify(elem['name'])
                default_flow_id = elem.get('default')
                then_flow_id = next((f for f in outgoing_flows if f != default_flow_id), None)
                then_proc = self._build_proc_for_element(process_data['sequence_flows'][then_flow_id]['target'], process_data, visited.copy()) if then_flow_id else "[]"
                else_proc = self._build_proc_for_element(process_data['sequence_flows'][default_flow_id]['target'], process_data, visited.copy()) if default_flow_id else "[]"
                return f"if({cond_fluent}(ID), {then_proc}, {else_proc})"
        elif 'eventBasedGateway' in elem['type']:
            branches = []
            for flow_id in outgoing_flows:
                target_id = process_data['sequence_flows'][flow_id]['target']
                catch_event = process_data['elements'][target_id]
                source_msg_id = next((k for k, v in self.parser.message_flows.items() if v == target_id), None)
                source_event = self._find_element_by_id(source_msg_id)
                wait_event_name = self._prologify(source_event['name'])
                rest_of_branch_id = process_data['sequence_flows'][catch_event['outgoing'][0]]['target']
                rest_of_branch_proc = self._build_proc_for_element(rest_of_branch_id, process_data, visited.copy())
                branches.append(f"[?(done({wait_event_name}(ID))), {rest_of_branch_proc}]")
            return "ndet(" + ", ".join(branches) + ")"
        elif 'parallelGateway' in elem['type']:
            join_gateway_id = self._find_join_gateway(elem_id, process_data)
            branch_starts = []
            branch_terminators = []
            
            # Get the first task in each branch
            for flow_id in outgoing_flows:
                branch_start_id = process_data['sequence_flows'][flow_id]['target']
                branch_start_elem = process_data['elements'][branch_start_id]
                if 'task' in branch_start_elem['type']:
                    branch_start_name = self._prologify(branch_start_elem['name'])
                    branch_starts.append(f"{branch_start_name}(start, ID)")
                    branch_terminators.append(f"done({branch_start_name}(end, ID))")
            
            # Build wait condition for all branches to complete
            if len(branch_terminators) > 1:
                wait_proc = f"?(and({', '.join(sorted(branch_terminators))}))"
            elif branch_terminators:
                wait_proc = f"?({branch_terminators[0]})"
            else:
                wait_proc = ""
            
            # Get what comes after the join
            join_elem = process_data['elements'][join_gateway_id]
            if join_elem['outgoing']:
                next_elem_id = process_data['sequence_flows'][join_elem['outgoing'][0]]['target']
                next_proc = self._build_proc_for_element(next_elem_id, process_data, visited)
            else:
                next_proc = "[]"
            
            # Build the concurrent structure
            conc_part = f"conc({', '.join(branch_starts)})" if len(branch_starts) > 1 else branch_starts[0] if branch_starts else "[]"
            
            if wait_proc and next_proc != "[]":
                return f"[{conc_part}, {wait_proc}, {next_proc}]"
            elif wait_proc:
                return f"[{conc_part}, {wait_proc}]"
            else:
                return conc_part
        
        next_elem_id = process_data['sequence_flows'][outgoing_flows[0]]['target']
        next_proc = self._build_proc_for_element(next_elem_id, process_data, visited)
        
        # Build sequence with proper flattening to avoid excessive nesting
        if not current_proc:
            return next_proc
        if next_proc == "[]":
            return current_proc
        
        # Check if current_proc or next_proc are lists (but not special constructs like [?(...)])
        current_is_list = current_proc.startswith('[') and not current_proc.startswith('[?')
        next_is_list = next_proc.startswith('[') and not next_proc.startswith('[?')
        
        if current_is_list and next_is_list:
            # Both are lists - merge: [a, b] + [c, d] -> [a, b, c, d]
            return f"[{current_proc[1:-1]}, {next_proc[1:-1]}]"
        elif current_is_list:
            # Current is list - append next: [a, b] + c -> [a, b, c]
            return f"[{current_proc[1:-1]}, {next_proc}]"
        elif next_is_list:
            # Next is list - prepend current: a + [b, c] -> [a, b, c]
            return f"[{current_proc}, {next_proc[1:-1]}]"
        else:
            # Neither is list - create new list: a + b -> [a, b]
            return f"[{current_proc}, {next_proc}]"

    def _get_all_path_conditions(self, start_node_id, process_data):
        paths = []
        q = [(start_node_id, [])]
        max_depth = 20
        
        while q and max_depth > 0:
            max_depth -= 1
            current_id, current_path_conditions = q.pop(0)
            
            elem = process_data['elements'].get(current_id)
            if not elem: continue

            predecessors = self._find_true_predecessors(current_id, process_data)
            if predecessors:
                pred_conds = []
                for p in predecessors:
                    if 'task' in p['type']:
                        pred_conds.append(f"done({self._prologify(p['name'])}(end, ID))")
                    elif 'intermediateThrowEvent' in p['type']:
                        pred_conds.append(f"done({self._prologify(p['name'])}(ID))")
                    elif 'intermediateCatchEvent' in p['type']:
                        # Catch events wait for their corresponding throw event
                        source_msg_id = next((k for k, v in self.parser.message_flows.items() if v == p['id']), None)
                        if source_msg_id:
                            source_elem = self._find_element_by_id(source_msg_id)
                            if source_elem:
                                pred_conds.append(f"done({self._prologify(source_elem['name'])}(ID))")
                    elif 'startEvent' in p['type']:
                        # Check if it's a message catch start event
                        is_message_catch = any(v == p['id'] for v in self.parser.message_flows.values())
                        if is_message_catch:
                            # Find the throw event
                            source_msg_id = next((k for k, v in self.parser.message_flows.items() if v == p['id']), None)
                            if source_msg_id:
                                source_elem = self._find_element_by_id(source_msg_id)
                                if source_elem:
                                    pred_conds.append(f"done({self._prologify(source_elem['name'])}(ID))")
                        else:
                            pred_conds.append(f"done({self._prologify(p['name'])}(ID))")
                    else:
                        pred_conds.append(f"done({self._prologify(p['name'])}(ID))")
                paths.append(current_path_conditions + pred_conds)
                continue

            if not elem.get('incoming'):
                if current_path_conditions: paths.append(current_path_conditions)
                continue

            for inc_flow_id in elem.get('incoming'):
                source_id = process_data['sequence_flows'][inc_flow_id]['source']
                source_elem = process_data['elements'].get(source_id)
                if not source_elem: continue

                new_path_conditions = list(current_path_conditions)
                if 'exclusiveGateway' in source_elem['type'] and source_elem['name']:
                    fluent = self._prologify(source_elem['name']) + "(ID)"
                    is_default = source_elem.get('default') == inc_flow_id
                    new_path_conditions.append(f"neg({fluent})" if is_default else fluent)
                    # STOP here - don't continue backward past a named exclusive gateway
                    # The fluent IS the precondition
                    paths.append(new_path_conditions)
                    continue
                q.append((source_id, new_path_conditions))
        
        return paths if paths else [[]]

    def _build_proc_for_branch(self, elem_id, stop_node_id, process_data, visited):
        proc_parts, terminators = [], []
        current_id = elem_id
        while current_id and current_id != stop_node_id:
            if current_id in visited: break
            visited.add(current_id)
            elem_proc = self._build_proc_for_element(current_id, process_data, set(visited))
            proc_parts.append(elem_proc)
            elem = process_data['elements'][current_id]
            if not elem['outgoing']:
                terminators.append(elem)
                break
            else:
                next_id = process_data['sequence_flows'][elem['outgoing'][0]]['target']
                if next_id == stop_node_id: terminators.append(elem)
                current_id = next_id
        return ("[" + ", ".join(proc_parts) + "]") if len(proc_parts) > 1 else proc_parts[0] if proc_parts else "[]", terminators
    
    def _find_true_predecessors(self, elem_id, process_data):
        predecessors, q, visited = [], [elem_id], set()
        while q:
            curr_id = q.pop(0)
            if curr_id in visited: continue
            visited.add(curr_id)
            elem = process_data['elements'].get(curr_id)
            if not elem: continue
            for inc_id in elem.get('incoming', []):
                source_id = process_data['sequence_flows'][inc_id]['source']
                source_elem = process_data['elements'].get(source_id)
                if not source_elem: continue
                # Named exclusive gateways are stopping points - they represent fluent conditions
                if 'exclusiveGateway' in source_elem['type'] and source_elem['name']:
                    # Don't add as predecessor, don't continue backward
                    pass
                # Other gateways - skip and continue searching backward
                elif 'gateway' in source_elem['type'].lower(): 
                    q.append(source_id)
                # Skip catch events - replace with their throw event
                elif 'intermediateCatchEvent' in source_elem['type']:
                    source_msg_id = next((k for k, v in self.parser.message_flows.items() if v == source_id), None)
                    if source_msg_id:
                        throw_elem = self._find_element_by_id(source_msg_id)
                        if throw_elem:
                            predecessors.append(throw_elem)
                # Regular elements
                else: 
                    predecessors.append(source_elem)
        return predecessors

    def _find_join_gateway(self, split_gateway_id, process_data):
        q = [process_data['sequence_flows'][f]['target'] for f in process_data['elements'][split_gateway_id]['outgoing']]
        visited = set(q)
        while q:
            curr_id = q.pop(0)
            curr_elem = process_data['elements'].get(curr_id)
            if curr_elem and 'parallelGateway' in curr_elem['type'] and len(curr_elem['incoming']) > 1: return curr_id
            if curr_elem:
                for f in curr_elem['outgoing']:
                    next_id = process_data['sequence_flows'][f]['target']
                    if next_id not in visited:
                        q.append(next_id)
                        visited.add(next_id)
        return None

    def _generate_initial_state(self, fluents):
        code = "\n% Initial Situation\n"
        code += "initially(pool(_P), true).\n\n"
        return code

    def _find_element_by_id(self, elem_id):
        for proc in self.parser.processes.values():
            if elem_id in proc['elements']: return proc['elements'][elem_id]
        return None

    def _find_pool_for_element(self, elem_id):
        for proc_id, process in self.parser.processes.items():
            if elem_id in process['elements']:
                return self.parser.get_participant_by_process_id(proc_id)
        return None

    def _is_in_subprocess(self, elem_id, process_data):
        for e in process_data['elements'].values():
            if 'subProcess' in e['type'] and elem_id in e.get('contained_elements', []): return True
        return False

    def _find_subprocess_start_event(self, subprocess_id, process_data):
        sub_proc = process_data['elements'].get(subprocess_id)
        if not sub_proc or 'contained_elements' not in sub_proc: return None
        for contained_id in sub_proc['contained_elements']:
            elem = process_data['elements'].get(contained_id)
            if elem and 'startEvent' in elem['type']: return elem
        return None
    
    def _is_decision_task(self, task_id, process_data):
        task = process_data['elements'].get(task_id)
        if not task or not task['outgoing']: return False
        next_elem_id = process_data['sequence_flows'][task['outgoing'][0]]['target']
        next_elem = process_data['elements'].get(next_elem_id)
        return next_elem and 'exclusiveGateway' in next_elem['type'] and next_elem['name']

    def _generate_footer(self):
        return """
prim_action(end_bpmn).
poss(end_bpmn, true).

proc(exog_actions,
  if(done(end_bpmn), [], [exog_action, exog_actions])).

% simulate process BP under exogenous events
proc(sim(BP), conc([BP, end_bpmn], exog_actions)).
proc(exog_action, pi(a, [?(and(exog_action(a), neg(system_action(a)))), a])).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  INFORMATION FOR THE EXECUTOR
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Translations of domain actions to real actions (one-to-one)
actionNum(X, X).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
"""