import re

class PrologTranslator:
    def __init__(self, parser):
        self.parser = parser

    def _prologify(self, name):
        if not name: return ""
        s = name.lower()
        s = re.sub(r'[\s-]+', '_', s)
        s = re.sub(r'[?\'"]', '', s)
        return s

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
    causes_false/3,
    poss/2,
    prim_action/1,
    exog_action/1.

cache(_) :- fail.

id(X) :- ground(X), !.
id(X) :- between(1, 1, X).

"""

    def _generate_fluents_and_causes(self):
        fluents = {'active(_ID)', 'servers_stopped', 'processed(_ID)'}
        causes = []
        
        pool_names = [self._prologify(p['name']) for p in self.parser.participants.values()]
        fluents.add(f"pool(P) :- member(P, [{', '.join(pool_names)}]).")
        
        fluents.add('waiting(_ID, _POOL)')
        causes.extend([
            "causes_false(acquire(ID, POOL), waiting(ID, POOL), true).",
            "causes_true(shut_down, servers_stopped, true).",
            "causes_true(application_analysed(ID), processed(ID), true).",
            "causes_true(withdrawal_handled(ID), processed(ID), true)."
        ])

        for source_id, target_id in self.parser.message_flows.items():
            source_elem = self._find_element_by_id(source_id)
            if source_elem:
                target_pool = self._find_pool_for_element(target_id)
                if target_pool:
                    source_action = self._prologify(source_elem['name'])
                    target_pool_name = self._prologify(target_pool['name'])
                    causes.append(f"causes_true({source_action}(ID), waiting(ID, {target_pool_name}), true).")

        for proc_id, process in self.parser.processes.items():
            pool_name = self._prologify(self.parser.get_participant_by_process_id(proc_id)['name'])
            for elem in process['elements'].values():
                elem_name = self._prologify(elem['name'])
                if 'startEvent' in elem['type'] and not self._is_in_subprocess(elem['id'], process):
                    causes.append(f"causes_true({elem_name}(ID), active(ID), true).")
                    causes.append(f"causes_true({elem_name}(ID), waiting(ID, {pool_name}), true).")
                if 'endEvent' in elem['type'] and elem_name:
                    causes.append(f"causes_false({elem_name}(ID), active(ID), true).")
                    causes.append(f"causes_false({elem_name}(ID), waiting(ID, {pool_name}), true).")

        self._add_data_and_gateway_fluents(fluents, causes)
        
        fluent_code = "\n% Domain-dependent Relational Fluents\n" + "\n".join([f"rel_fluent({f})." for f in sorted(list(fluents)) if ':-' not in f]) + "\n" + "\n".join([f for f in fluents if ':-' in f]) + "\n\n"
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
                    if do_id in elem.get('data_outputs', []):
                        causes.append(f"causes_true({self._prologify(elem['name'])}(end, ID), {fluent_name}(ID), true).")

    def _generate_actions_and_preconditions(self):
        code = "\n% Actions and Preconditions\n"
        action_defs, exog_actions = [], {'shut_down'}

        for proc_id, process in self.parser.processes.items():
            for elem_id, elem in process['elements'].items():
                elem_name = self._prologify(elem['name'])
                if not elem_name or 'gateway' in elem['type']: continue

                special_preconditions = {
                    'withdrawal_by_applicant': 'and(active(ID), and(neg(processed(ID)), neg(done(withdrawal_by_applicant(ID)))))',
                    'withdrawal_handled': 'and(done(withdrawal_completed(ID)), done(process_withdrawal(end, ID)))',
                    'application_analysed': 'or(done(store_signed_contract(end, ID)), done(letter_of_refusal_sent(ID)))'
                }
                if elem_name in special_preconditions:
                    poss_cond = special_preconditions[elem_name]
                else:
                    path_conditions = self._get_all_path_conditions(elem_id, process)
                    data_conditions = [self._prologify(self.parser.data_objects[do_id]['name']) + "(ID)" for do_id in elem.get('data_inputs', []) if do_id in self.parser.data_objects]
                    
                    final_conditions = []
                    for path in path_conditions:
                        all_conds = sorted(list(set(path + data_conditions)))
                        if len(all_conds) > 1: final_conditions.append("and(" + ", ".join(all_conds) + ")")
                        elif all_conds: final_conditions.append(all_conds[0])
                    
                    if len(final_conditions) > 1: poss_cond = "or(" + ", ".join(sorted(list(set(final_conditions)))) + ")"
                    elif final_conditions: poss_cond = final_conditions[0]
                    else: poss_cond = "true"

                if 'task' in elem['type']:
                    action_defs.append(f"prim_action({elem_name}(start, _ID)).\nposs({elem_name}(start, ID), {poss_cond}).")
                    exog_actions.add(f"{elem_name}(end, _ID, _)") if self._is_decision_task(elem_id, process) else exog_actions.add(f"{elem_name}(end, _ID)")
                elif 'Event' in elem['type'] and ('intermediate' in elem['type'] or 'end' in elem['type']):
                     action_defs.append(f"prim_action({elem_name}(_ID)).\nposs({elem_name}(ID), {poss_cond}).")
                elif 'startEvent' in elem['type']: exog_actions.add(f"{elem_name}(_ID)")
        
        code += "\n".join(sorted(list(set(action_defs)))) + "\n"
        code += "\n% Exogenous Actions\nprim_action(Act) :- exog_action(Act).\n"
        for exog in sorted(list(exog_actions)):
            base_name = exog.split('(')[0]
            code += f"exog_action({exog}).\n"
            if 'end' in exog: code += f"poss({exog.replace('_ID, _', 'ID, _').replace('_ID', 'ID')}, running({base_name}(start, ID))).\n"
            elif 'shut_down' not in exog: code += f"poss({base_name}(ID), neg(done({base_name}(ID)))).\n"
        code += "poss(shut_down, neg(servers_stopped)).\n"
        
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
        code += "  A1 =.. [F|[start|L]], A2 =.. [F|[end|L]].\n\n"
        
        code += "proc(active_instances_check, some(id, active(id))).\n"
        
        return code

    def _generate_procedures(self):
        code = "\n% HIGH-LEVEL PROCESS PROCEDURES\n"
        participant_procs = [self._prologify(p['name']) for p in self.parser.participants.values()]
        code += f"proc(control(bpmn_process), conc({', '.join([f'server_{p}' for p in participant_procs])})).\n\n"

        for proc_id, process_data in self.parser.processes.items():
            p_name = self._prologify(self.parser.get_participant_by_process_id(proc_id)['name'])
            
            main_start_id, event_subprocesses = None, []
            for elem_id, elem in process_data['elements'].items():
                if 'startEvent' in elem['type'] and not self._is_in_subprocess(elem_id, process_data): main_start_id = elem_id
                if elem.get('is_event_subprocess'): event_subprocesses.append(elem)

            main_flow_proc_name = f"{p_name}_procedure"
            main_flow_proc = self._build_proc_for_element(main_start_id, process_data)
            code += f"proc({main_flow_proc_name}(ID), {main_flow_proc}).\n"

            if not event_subprocesses:
                handle_proc_body = f"{main_flow_proc_name}(ID)"
            else:
                sub_proc = event_subprocesses[0]
                sub_proc_start_event = self._find_subprocess_start_event(sub_proc['id'], process_data)
                trigger_event_name = self._prologify(sub_proc_start_event['name'])
                sub_proc_body = self._build_proc_for_element(sub_proc_start_event['id'], process_data)
                
                if p_name == 'applicant':
                    gexec_cond = f"and(active(ID), neg(done({trigger_event_name}(ID))))"
                    if_cond = f"and(neg(processed(ID)), done({trigger_event_name}(ID)))"
                else: # company
                    gexec_cond = f"and(neg(processed(ID)), neg(done(withdrawal_sent(ID))))"
                    if_cond = f"and(neg(processed(ID)), done(withdrawal_sent(ID)))"
                
                handle_proc_body = f"[ gexec({gexec_cond}, {main_flow_proc_name}(ID)), if({if_cond}, {sub_proc_body}, []) ]"
            
            code += f"proc(handle_{p_name}(ID), {handle_proc_body}).\n"
            code += f"proc(server_{p_name}, pi(app, [acquire(app, {p_name}), handle_{p_name}(app)])).\n\n"
        return code

    def _build_proc_for_element(self, elem_id, process_data, visited=None):
        if visited is None: visited = set()
        if not elem_id or elem_id in visited: return "[]"
        visited.add(elem_id)
        elem = process_data['elements'][elem_id]
        elem_name = self._prologify(elem['name'])
        
        if 'startEvent' in elem['type']: current_proc = f"?(done({elem_name}(ID)))"
        elif 'task' in elem['type']:
            wait_action = f"?(some(res, done({elem_name}(end, ID, res))))" if self._is_decision_task(elem_id, process_data) else f"?(done({elem_name}(end, ID)))"
            current_proc = f"[{elem_name}(start, ID), {wait_action}]"
        elif 'intermediateThrowEvent' in elem['type']: current_proc = f"{elem_name}(ID)"
        elif 'endEvent' in elem['type']: return f"{elem_name}(ID)" if elem_name else "[]"
        elif 'intermediateCatchEvent' in elem['type']:
            source_msg_id = next((k for k, v in self.parser.message_flows.items() if v == elem_id), None)
            source_elem = self._find_element_by_id(source_msg_id) if source_msg_id else None
            current_proc = f"?(done({self._prologify(source_elem['name'])}(ID)))" if source_elem else "[]"
        else: current_proc = ""
        
        outgoing_flows = elem.get('outgoing', [])
        if not outgoing_flows: return current_proc or "[]"
        
        if 'exclusiveGateway' in elem['type']:
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
            branches, branch_terminators = [], []
            for flow_id in outgoing_flows:
                branch_start_id = process_data['sequence_flows'][flow_id]['target']
                branch_proc, terminators = self._build_proc_for_branch(branch_start_id, join_gateway_id, process_data, set(visited))
                branches.append(branch_proc)
                branch_terminators.extend(terminators)
            wait_conditions = [f"done({self._prologify(t['name'])}(end, ID))" for t in branch_terminators]
            wait_proc = f"?(and({', '.join(sorted(wait_conditions))}))" if len(wait_conditions) > 1 else f"?(done({self._prologify(branch_terminators[0]['name'])}(end, ID)))" if branch_terminators else ""
            next_elem_id = process_data['sequence_flows'][process_data['elements'][join_gateway_id]['outgoing'][0]]['target']
            next_proc = self._build_proc_for_element(next_elem_id, process_data, visited)
            return f"[conc({', '.join(branches)}), {wait_proc}, {next_proc}]"
        
        next_elem_id = process_data['sequence_flows'][outgoing_flows[0]]['target']
        next_proc = self._build_proc_for_element(next_elem_id, process_data, visited)
        return f"[{current_proc}, {next_proc}]" if current_proc and next_proc != "[]" else current_proc or next_proc

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
                pred_conds = [f"done({self._prologify(p['name'])}(end, ID))" if 'task' in p['type'] else f"done({self._prologify(p['name'])}(ID))" for p in predecessors]
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
                if 'gateway' in source_elem['type']: q.append(source_id)
                else: predecessors.append(source_elem)
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
        initially_false = [f"initially({f}, false)." for f in fluents if 'pool' not in f and ':-' not in f]
        code += "\n".join(sorted(initially_false)) + "\n"
        code += "initially(pool(_P), true).\n"
        code += "initially(servers_stopped, false).\n"
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
prim_action(end_bpm).
poss(end_bpm, true).

proc(exog_actions,
  if(done(end_bpm), [], [exog_action, exog_actions])).

proc(exog_action, pi(a, [?(and(exog_action(a), neg(system_action(a)))), a])).

actionNum(X, X).
"""