import re
from prolog_templates import *

START_EVENT = 'startEvent'
END_EVENT = 'endEvent'
INTERMEDIATE_THROW_EVENT = 'intermediateThrowEvent'
INTERMEDIATE_CATCH_EVENT = 'intermediateCatchEvent'
TASK = 'task'
EXCLUSIVE_GATEWAY = 'exclusiveGateway'
PARALLEL_GATEWAY = 'parallelGateway'
EVENT_BASED_GATEWAY = 'eventBasedGateway'
GATEWAY = 'gateway'
SUBPROCESS = 'subProcess'

class PrologTranslator:
    def __init__(self, parser):
        self.parser = parser

    def _prologify(self, name):
        if not name: return ""
        s = name.lower()
        # Replace comparison operators with words before other replacements
        s = s.replace('>', 'larger')
        s = s.replace('<', 'smaller')
        s = s.replace('=', 'equal')
        # Replace spaces and hyphens with underscores
        s = re.sub(r'[\s-]+', '_', s)
        # Remove or replace invalid Prolog atom characters
        # Valid Prolog atom chars: letters, digits, underscore
        # Remove: ?, ', ", !, @, #, $, %, ^, *, +, ~, `, |, \, /, :, ;, &, (, ), [, ], {, }, ,, .
        s = re.sub(r'[?\'\"!@#$%^*+~`|\\/:;&\(\)\[\]{},.]', '', s)
        if s.startswith('send_'):
            s = s.replace('send_', '', 1) + '_sent'
        # Ensure the result is a valid Prolog atom (starts with lowercase letter)
        # If it starts with underscore or digit, prepend a letter prefix
        if s and not s[0].islower():
            if s[0].isdigit():
                s = 'n' + s  # 'n' for numeric
            elif s[0] == '_':
                s = 'v' + s  # 'v' for value
            else:
                s = 'x' + s  # 'x' for other
        return s
    
    def _is_element_type(self, elem, element_type):
        """Check if an element matches a specific type (case-insensitive)."""
        return elem and element_type.lower() in elem.get('type', '').lower()
    
    def _is_message_catch(self, elem_id):
        """Check if an element is a message catch event (target of a message flow)."""
        return any(v == elem_id for v in self.parser.message_flows.values())
    
    def _find_throw_event_for_catch(self, catch_elem_id):
        """Find the throw event that corresponds to a catch event via message flow."""
        source_msg_id = next((k for k, v in self.parser.message_flows.items() if v == catch_elem_id), None)
        if source_msg_id:
            return self._find_element_by_id(source_msg_id)
        return None
    
    def _get_done_condition_for_element(self, elem):
        """Generate the done() condition for a predecessor element."""
        if self._is_element_type(elem, TASK):
            return f"done({self._prologify(elem['name'])}(end, ID))"
        elif self._is_element_type(elem, INTERMEDIATE_THROW_EVENT):
            return f"done({self._prologify(elem['name'])}(ID))"
        elif self._is_element_type(elem, INTERMEDIATE_CATCH_EVENT):
            source_elem = self._find_throw_event_for_catch(elem['id'])
            if source_elem:
                return f"done({self._prologify(source_elem['name'])}(ID))"
        elif self._is_element_type(elem, START_EVENT):
            if self._is_message_catch(elem['id']):
                source_elem = self._find_throw_event_for_catch(elem['id'])
                if source_elem:
                    return f"done({self._prologify(source_elem['name'])}(ID))"
            return f"done({self._prologify(elem['name'])}(ID))"
        else:
            return f"done({self._prologify(elem['name'])}(ID))"
        return None
    
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
    
    def _build_nested_or(self, conditions):
        """Build nested binary or/2 predicates from a list of conditions."""
        if not conditions:
            return "false"
        if len(conditions) == 1:
            return conditions[0]
        if len(conditions) == 2:
            return f"or({conditions[0]}, {conditions[1]})"
        # For more than 2, nest recursively: or(C1, or(C2, or(C3, ...)))
        return f"or({conditions[0]}, {self._build_nested_or(conditions[1:])})"

    def _interleave_fluents_and_causes(self, fluents, causes):
        """
        Organize fluents and their causal laws together, matching the structure of job.pl.
        Returns a string with fluents immediately followed by their causes.
        Handles both relational (rel_fluent) and functional (fun_fluent) fluents.
        """
        # Extract fluent names from causes to group them
        fluent_to_causes = {}
        
        for cause in causes:
            # Parse cause to extract fluent name
            # Format: causes_true(action(...), fluent_name(ID), condition).
            # or: causes_false(action(...), fluent_name(ID), condition).
            # or: causes_val(action(...), fluent_name(ID), VALUE, condition).
            # or: causes_true(action, fluent_name, condition). (for simple fluents like servers_stopped)
            import re
            # Match the fluent name (second argument), handling both fluent(ID) and simple fluent forms
            match = re.search(r'causes_(?:true|false|val)\(.+?,\s*([a-z_]+)[\(,]', cause)
            if match:
                fluent_name = match.group(1)
                if fluent_name not in fluent_to_causes:
                    fluent_to_causes[fluent_name] = []
                fluent_to_causes[fluent_name].append(cause)
        
        # Generate output with fluents followed by their causes
        result = []
        
        # Separate functional, relational, and rule-based fluents
        functional_fluents = [f for f in fluents if f.startswith('fun:') and ':-' not in f]
        relational_fluents = [f for f in fluents if not f.startswith('fun:') and ':-' not in f]
        rule_fluents = [f for f in fluents if ':-' in f]
        
        # Process functional fluents first
        for fluent in sorted(functional_fluents):
            # Remove the 'fun:' prefix
            fluent_decl = fluent[4:]  # Remove 'fun:'
            fluent_name = fluent_decl.split('(')[0] if '(' in fluent_decl else fluent_decl
            
            # Add the functional fluent declaration
            result.append(f"fun_fluent({fluent_name}(_ID)).")
            
            # Add its causal laws immediately after
            if fluent_name in fluent_to_causes:
                for cause in sorted(fluent_to_causes[fluent_name]):
                    result.append(cause)
        
        # Process relational fluents
        for fluent in sorted(relational_fluents):
            # Extract fluent name (e.g., "active(_ID, _POOL)" -> "active")
            fluent_name = fluent.split('(')[0] if '(' in fluent else fluent
            
            # Add the fluent declaration
            result.append(f"rel_fluent({fluent}).")
            
            # Add its causal laws immediately after
            if fluent_name in fluent_to_causes:
                for cause in sorted(fluent_to_causes[fluent_name]):
                    result.append(cause)
        
        # Add rule-based fluents (like pool(P)) at the end
        result.extend(rule_fluents)
        
        return "\n".join(result)

    def translate(self):
        code = self._generate_header()
        code += self._generate_fluents_and_causes()
        code += self._generate_actions_and_preconditions()
        code += self._generate_procedures()
        code += self._generate_footer()
        return code

    def _generate_header(self):
        return PROLOG_HEADER

    def _generate_fluents_and_causes(self):
        fluents = set(STANDARD_FLUENTS)
        causes = list(STANDARD_CAUSES)  # Already includes acquire and shut_down causes
        
        pool_names = [self._prologify(p['name']) for p in self.parser.participants.values()]
        fluents.add(f"rel_fluent(pool(P)) :- member(P, [{', '.join(pool_names)}]).")
        fluents.add('waiting(_ID, _POOL)')

        for source_id, target_id in self.parser.message_flows.items():
            source_elem = self._find_element_by_id(source_id)
            target_elem = self._find_element_by_id(target_id)
            if not (source_elem and target_elem and self._is_element_type(target_elem, START_EVENT)):
                continue
            
            target_process = next((proc for proc in self.parser.processes.values() if target_id in proc['elements']), None)
            if target_process and not self._is_in_subprocess(target_id, target_process):
                target_pool = self._find_pool_for_element(target_id)
                if target_pool:
                    source_action = self._prologify(source_elem['name'])
                    target_pool_name = self._prologify(target_pool['name'])
                    causes.append(f"causes_true({source_action}(ID), waiting(ID, {target_pool_name}), true).")

        for proc_id, process in self.parser.processes.items():
            pool_name = self._prologify(self.parser.get_participant_by_process_id(proc_id)['name'])
            for elem in process['elements'].values():
                elem_name = self._prologify(elem['name'])
                elem_id = elem['id']
                
                if self._is_element_type(elem, START_EVENT) and not self._is_in_subprocess(elem_id, process):
                    if not self._is_message_catch(elem_id):
                        causes.append(f"causes_true({elem_name}(ID), waiting(ID, {pool_name}), true).")
                
                if self._is_element_type(elem, END_EVENT) and elem_name:
                    is_in_subprocess = self._is_in_subprocess(elem_id, process)
                    is_terminate = elem.get('is_terminate', False)
                    
                    if not is_in_subprocess:
                        causes.append(f"causes_false({elem_name}(ID), active(ID, {pool_name}), true).")
                        causes.append(f"causes_false({elem_name}(ID), waiting(ID, {pool_name}), true).")
                    elif is_in_subprocess and is_terminate:
                        causes.append(f"causes_false({elem_name}(ID), active(ID, {pool_name}), true).")

        self._add_data_and_gateway_fluents(fluents, causes)
        
        unique_causes = list(dict.fromkeys(causes))
        
        interleaved_code = "\n% Domain-dependent Relational Fluents\n" + self._interleave_fluents_and_causes(fluents, unique_causes) + "\n\n"
        init_code = self._generate_initial_state(fluents)
        return interleaved_code + init_code

    def _analyze_gateway_branches(self, gateway_id, process_data):
        """
        Analyze an exclusive gateway to determine if it needs a functional fluent.
        Returns: (is_functional, branch_values_dict)
          - is_functional: True if branches have explicit non-boolean labels OR >2 branches
          - branch_values_dict: {flow_id: prologified_value}
        """
        gateway_elem = process_data['elements'][gateway_id]
        outgoing_flows = gateway_elem.get('outgoing', [])
        default_flow = gateway_elem.get('default')
        
        # Get labels for each outgoing flow
        branch_values = {}
        labeled_count = 0
        has_non_boolean_labels = False
        
        for flow_id in outgoing_flows:
            flow_info = process_data['sequence_flows'].get(flow_id, {})
            flow_label = flow_info.get('name', '').strip()
            
            if flow_id == default_flow:
                # Default branch gets a special value
                branch_values[flow_id] = 'default'
            elif flow_label:
                # Non-empty label - create a Prolog atom from it
                labeled_count += 1
                prologified = self._prologify(flow_label)
                branch_values[flow_id] = prologified
                
                # Check if this is a non-boolean label (not "yes", "no", "true", "false")
                label_lower = flow_label.lower().strip()
                if label_lower not in ['yes', 'no', 'true', 'false', '']:
                    has_non_boolean_labels = True
            else:
                # Empty label - use flow index
                branch_values[flow_id] = f'branch_{len(branch_values)}'
        
        # Use functional fluent if:
        # 1. More than 2 branches with distinct labels, OR
        # 2. Exactly 2 branches but with explicit non-boolean labels (e.g., "stay" vs "go back")
        is_functional = (len(outgoing_flows) > 2 and labeled_count >= 2) or \
                       (len(outgoing_flows) == 2 and has_non_boolean_labels and labeled_count >= 1)
        
        return is_functional, branch_values

    def _add_data_and_gateway_fluents(self, fluents, causes):
        for proc in self.parser.processes.values():
            for elem in proc['elements'].values():
                if self._is_element_type(elem, EXCLUSIVE_GATEWAY) and elem['name']:
                    fluent_name = self._prologify(elem['name'])
                    
                    # Analyze if this gateway needs a functional fluent
                    is_functional, branch_values = self._analyze_gateway_branches(elem['id'], proc)
                    
                    if is_functional:
                        # Functional fluent for multi-branch gateways
                        fluents.add(f"fun:{fluent_name}(_ID)")  # Mark as functional with prefix
                        predecessors = self._find_true_predecessors(elem['id'], proc)
                        for pred_task in predecessors:
                            if self._is_element_type(pred_task, TASK):
                                task_end = f"{self._prologify(pred_task['name'])}(end, ID, VALUE)"
                                # Generate causes_val for each possible branch value
                                # The task returns the value that determines which branch to take
                                causes.append(f"causes_val({task_end}, {fluent_name}(ID), VALUE, true).")
                    else:
                        # Relational fluent for binary gateways (traditional true/false)
                        fluents.add(f"{fluent_name}(_ID)")
                        predecessors = self._find_true_predecessors(elem['id'], proc)
                        for pred_task in predecessors:
                            if self._is_element_type(pred_task, TASK):
                                task_end = f"{self._prologify(pred_task['name'])}(end, ID, RESULT)"
                                causes.extend([
                                    f"causes_true({task_end}, {fluent_name}(ID), RESULT = true).",
                                    f"causes_false({task_end}, {fluent_name}(ID), RESULT = false)."
                                ])
        
        for do_id, do_info in self.parser.data_objects.items():
            fluent_name = self._prologify(do_info['name'])
            fluents.add(f"{fluent_name}(_ID)")
            for proc in self.parser.processes.values():
                for elem in proc['elements'].values():
                    if self._is_element_type(elem, TASK) and do_id in elem.get('data_outputs', []):
                        causes.append(f"causes_true({self._prologify(elem['name'])}(end, ID), {fluent_name}(ID), true).")

    def _generate_actions_and_preconditions(self):
        code = ACTIONS_PRECONDITIONS_HEADER
        action_defs = []
        exog_actions_list = []

        for proc_id, process in self.parser.processes.items():
            for elem_id, elem in process['elements'].items():
                elem_name = self._prologify(elem['name'])
                if not elem_name or GATEWAY in elem['type']: 
                    continue

                path_conditions = self._get_all_path_conditions(elem_id, process)
                data_conditions = [self._prologify(self.parser.data_objects[do_id]['name']) + "(ID)" 
                                 for do_id in elem.get('data_inputs', []) if do_id in self.parser.data_objects]
                
                final_conditions = []
                for path in path_conditions:
                    all_conds_set = list(set(path + data_conditions))
                    done_conds = [c for c in all_conds_set if c.startswith('done(')]
                    other_conds = [c for c in all_conds_set if not c.startswith('done(')]
                    all_conds = sorted(done_conds) + sorted(other_conds)
                    
                    has_done_conditions = any(cond.startswith('done(') for cond in all_conds)
                    
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
                    unique_conditions_list = list(set(final_conditions))
                    if all(c.startswith('neg(') and c.endswith(')') for c in unique_conditions_list):
                        inner_conds = [c[4:-1] for c in unique_conditions_list]
                        inner_conds_sorted = sorted(inner_conds, key=lambda x: (not x.endswith('(ID)'), x))
                        poss_cond = "neg(" + self._build_nested_and(inner_conds_sorted) + ")"
                    else:
                        poss_cond = self._build_nested_or(sorted(unique_conditions_list))
                elif final_conditions: 
                    poss_cond = final_conditions[0]
                else: 
                    poss_cond = "true"

                if self._is_element_type(elem, TASK):
                    action_defs.append(f"prim_action({elem_name}(start, _ID)).\nposs({elem_name}(start, ID), {poss_cond}).")
                    end_poss_parts = [f"running({elem_name}(start, ID))"] + data_conditions
                    end_poss = self._build_nested_and(end_poss_parts) if len(end_poss_parts) > 1 else end_poss_parts[0]
                    
                    if self._is_decision_task(elem_id, process):
                        is_functional, decision_values = self._get_decision_values(elem_id, process)
                        # Format the values list as a Prolog list
                        values_str = '[' + ', '.join(decision_values) + ']'
                        exog_actions_list.append((f"{elem_name}(end, ID, RESULT)", f"{elem_name}(end, ID, _RESULT)", f"id(ID), member(RESULT, {values_str})", end_poss))
                    else:
                        exog_actions_list.append((f"{elem_name}(end, ID)", f"{elem_name}(end, ID)", "id(ID)", end_poss))
                
                elif self._is_element_type(elem, INTERMEDIATE_THROW_EVENT):
                    action_defs.append(f"prim_action({elem_name}(_ID)).\nposs({elem_name}(ID), {poss_cond}).")
                
                elif self._is_element_type(elem, INTERMEDIATE_CATCH_EVENT):
                    pass
                
                elif self._is_element_type(elem, END_EVENT):
                    is_in_subprocess = self._is_in_subprocess(elem_id, process)
                    is_terminate = elem.get('is_terminate', False)
                    
                    if is_in_subprocess and is_terminate:
                        action_defs.append(f"prim_action({elem_name}(_ID)).\nposs({elem_name}(ID), {poss_cond}).")
                    elif not is_in_subprocess:
                        exception_trigger = self._find_exception_trigger_event(proc_id)
                        if exception_trigger:
                            exception_trigger_name = self._prologify(exception_trigger['name'])
                            if poss_cond == "true":
                                enhanced_poss_cond = f"neg(done({exception_trigger_name}(ID)))"
                            else:
                                enhanced_poss_cond = f"and(neg(done({exception_trigger_name}(ID))), {poss_cond})"
                            action_defs.append(f"prim_action({elem_name}(_ID)).\nposs({elem_name}(ID), {enhanced_poss_cond}).")
                        else:
                            action_defs.append(f"prim_action({elem_name}(_ID)).\nposs({elem_name}(ID), {poss_cond}).")
                
                elif self._is_element_type(elem, START_EVENT):
                    if self._is_message_catch(elem_id):
                        pass
                    elif self._is_in_subprocess(elem_id, process):
                        precondition = self._generate_exception_trigger_precondition(elem_id, elem_name, proc_id, process)
                        exog_actions_list.append((f"{elem_name}(ID)", f"{elem_name}(ID)", "id(ID)", precondition))
                    else:
                        exog_actions_list.append((f"{elem_name}(ID)", f"{elem_name}(ID)", "id(ID)", f"neg(done({elem_name}(ID)))"))

        # Add acquire action
        action_defs.append(ACQUIRE_ACTION.strip())
        
        code += "\n".join(sorted(list(set(action_defs)))) + "\n"
        
        # Generate exogenous actions section
        code += EXOGENOUS_ACTIONS_HEADER
        code += EXOGENOUS_ACTION_CLAUSE
        
        for exog_decl, exog_poss_head, exog_guard, exog_poss_body in sorted(exog_actions_list):
            code += f"exog_action({exog_decl}) :- {exog_guard}.\n"
            code += f"poss({exog_poss_head}, {exog_poss_body}).\n\n"
        
        # Add shut_down
        code += SHUTDOWN_EXOG_ACTION
        
        # Append the new dynamic running procedure
        code += self._generate_running_proc_definition()
        
        return code

    def _generate_running_proc_definition(self):
        """Generates the dynamic running/1 procedure."""
        decision_task_starts = []
        for proc_id, process in self.parser.processes.items():
            for elem_id, elem in process['elements'].items():
                if self._is_element_type(elem, TASK) and self._is_decision_task(elem_id, process):
                    elem_name = self._prologify(elem['name'])
                    decision_task_starts.append(f"{elem_name}(start, _)")
        
        code = ABBREVIATIONS_HEADER
        if decision_task_starts:
            decision_tasks_str = ",\n      ".join(sorted(decision_task_starts))
            code += RUNNING_PROC_DECISION_TASKS.format(decision_tasks=decision_tasks_str)
        
        code += RUNNING_PROC_DEFAULT + ACTIVE_INSTANCES_CHECK
        return code

    def _generate_procedures(self):
        code = "\n"
        
        # Top-level controller
        code += TOP_LEVEL_CONTROLLER_COMMENT
        code += CONTROL_BPMN_PROCESS
        
        participant_procs = [self._prologify(p['name']) for p in self.parser.participants.values()]
        
        # Generate bpmn_process procedure
        if len(participant_procs) == 1:
            # Single pool: no need for conc()
            code += f"proc(bpmn_process, server_{participant_procs[0]}).\n\n\n"
        else:
            # Multiple pools: use conc()
            servers = ', '.join([f'server_{p}' for p in participant_procs])
            code += BPMN_PROCESS_CONC.format(servers=servers)

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
            code += SERVER_COMMENT.format(pool_upper=p_name.upper(), pool_name=p_name)
            code += SERVER_PROC.format(pool_name=p_name)
            
            # Build main flow procedure
            main_flow_proc = self._build_proc_for_element(main_start_id, process_data)
            code += POOL_PROC.format(pool_name=p_name, proc_body=main_flow_proc)

            # Build handle procedure with event subprocess support
            if not event_subprocesses:
                handle_proc_body = f"{p_name}(ID)"
            else:
                sub_proc = event_subprocesses[0]
                sub_proc_start_event = self._find_subprocess_start_event(sub_proc['id'], process_data)
                trigger_event_name = self._prologify(sub_proc_start_event['name'])
                
                # For the subprocess body, skip the start event and go directly to what follows it
                # The start event condition is already checked in the if(...) guard
                if sub_proc_start_event.get('outgoing'):
                    next_elem_id = process_data['sequence_flows'][sub_proc_start_event['outgoing'][0]]['target']
                    sub_proc_body = self._build_proc_for_element(next_elem_id, process_data)
                else:
                    sub_proc_body = "[]"
                
                # Determine the event to use in conditions
                # Check if the start event is a message catch (target of a message flow)
                start_event_id = sub_proc_start_event['id']
                is_message_catch = any(v == start_event_id for v in self.parser.message_flows.values())
                
                if is_message_catch:
                    # For message catch start events, find the corresponding throw event
                    source_msg_id = next((k for k, v in self.parser.message_flows.items() if v == start_event_id), None)
                    if source_msg_id:
                        source_elem = self._find_element_by_id(source_msg_id)
                        if source_elem:
                            condition_event_name = self._prologify(source_elem['name'])
                        else:
                            condition_event_name = trigger_event_name
                    else:
                        condition_event_name = trigger_event_name
                    # Message catch: check same pool in if condition
                    if_pool = p_name
                else:
                    # For non-message start events (error, escalation, etc.), use the trigger itself
                    condition_event_name = trigger_event_name
                    # Non-message: find the target pool of the message sent within subprocess
                    # Look for message flows originating from this subprocess
                    target_pool = None
                    for contained_id in sub_proc.get('contained_elements', []):
                        if contained_id in self.parser.message_flows:
                            target_elem_id = self.parser.message_flows[contained_id]
                            target_pool_obj = self._find_pool_for_element(target_elem_id)
                            if target_pool_obj:
                                target_pool = self._prologify(target_pool_obj['name'])
                                break
                    if_pool = target_pool if target_pool else p_name
                
                gexec_cond = f"and(active(ID, {p_name}), neg(done({condition_event_name}(ID))))"
                if_cond = f"and(active(ID, {if_pool}), done({condition_event_name}(ID)))"
                
                handle_proc_body = f"[ gexec({gexec_cond}, {p_name}(ID)),\n    if({if_cond},\n          {sub_proc_body},\n          []\n      )\n  ]"
            
            code += HANDLE_PROC.format(pool_name=p_name, proc_body=handle_proc_body)
        
        return code

    def _build_proc_for_element(self, elem_id, process_data, visited=None):
        if visited is None: visited = set()
        if not elem_id or elem_id in visited: return "[]"
        visited.add(elem_id)
        elem = process_data['elements'][elem_id]
        elem_name = self._prologify(elem['name'])
        
        if self._is_element_type(elem, START_EVENT):
            if self._is_message_catch(elem_id):
                source_elem = self._find_throw_event_for_catch(elem_id)
                if source_elem:
                    source_name = self._prologify(source_elem['name'])
                    wait_proc = f"?(done({source_name}(ID)))"
                    outgoing_flows = elem.get('outgoing', [])
                    if outgoing_flows:
                        next_elem_id = process_data['sequence_flows'][outgoing_flows[0]]['target']
                        next_proc = self._build_proc_for_element(next_elem_id, process_data, visited)
                        if next_proc and next_proc != "[]":
                            if next_proc.startswith('[') and next_proc.endswith(']'):
                                inner = next_proc[1:-1]
                                return f"[{wait_proc}, {inner}]"
                            else:
                                return f"[{wait_proc}, {next_proc}]"
                        else:
                            return wait_proc
                    return wait_proc
            
            # For regular start events (not message catch), wait for the exogenous start event
            # before proceeding with the rest of the process
            if elem_name:  # Only if start event has a name
                wait_proc = f"?(done({elem_name}(ID)))"
                outgoing_flows = elem.get('outgoing', [])
                if outgoing_flows:
                    next_elem_id = process_data['sequence_flows'][outgoing_flows[0]]['target']
                    next_proc = self._build_proc_for_element(next_elem_id, process_data, visited)
                    if next_proc and next_proc != "[]":
                        if next_proc.startswith('[') and next_proc.endswith(']'):
                            inner = next_proc[1:-1]
                            return f"[{wait_proc}, {inner}]"
                        else:
                            return f"[{wait_proc}, {next_proc}]"
                    else:
                        return wait_proc
                return wait_proc
            
            # If start event has no name, skip it and go to next element
            outgoing_flows = elem.get('outgoing', [])
            if outgoing_flows:
                next_elem_id = process_data['sequence_flows'][outgoing_flows[0]]['target']
                return self._build_proc_for_element(next_elem_id, process_data, visited)
            else:
                return "[]"
        elif self._is_element_type(elem, TASK):
            wait_action = f"?(some(res, done({elem_name}(end, ID, res))))" if self._is_decision_task(elem_id, process_data) else f"?(done({elem_name}(end, ID)))"
            current_proc = f"[{elem_name}(start, ID), {wait_action}]"
        elif self._is_element_type(elem, INTERMEDIATE_THROW_EVENT): 
            current_proc = f"{elem_name}(ID)"
        elif self._is_element_type(elem, END_EVENT): 
            return f"{elem_name}(ID)" if elem_name else "[]"
        elif self._is_element_type(elem, INTERMEDIATE_CATCH_EVENT):
            # Check if this is a timer event (has timerEventDefinition child)
            is_timer = any('timerEventDefinition' in str(child) for child in elem.get('children', []))
            
            if is_timer and elem_name:
                # Treat timer events as tasks: they have start/end actions
                wait_action = f"?(done({elem_name}(end, ID)))"
                current_proc = f"[{elem_name}(start, ID), {wait_action}]"
            else:
                # For message/signal catch events, look for corresponding throw event
                source_elem = self._find_throw_event_for_catch(elem_id)
                if source_elem:
                    source_name = self._prologify(source_elem['name'])
                    current_proc = f"?(done({source_name}(ID)))"
                else:
                    # If no throw event found and not a timer, treat as a task if it has a name
                    if elem_name:
                        wait_action = f"?(done({elem_name}(end, ID)))"
                        current_proc = f"[{elem_name}(start, ID), {wait_action}]"
                    else:
                        current_proc = "[]"
        else: 
            current_proc = ""
        
        outgoing_flows = elem.get('outgoing', [])
        if not outgoing_flows: return current_proc or "[]"
        
        if self._is_element_type(elem, EXCLUSIVE_GATEWAY):
            incoming_flows = elem.get('incoming', [])
            is_merge = len(incoming_flows) > 1 and len(outgoing_flows) == 1
            
            if is_merge:
                next_elem_id = process_data['sequence_flows'][outgoing_flows[0]]['target']
                return self._build_proc_for_element(next_elem_id, process_data, visited)
            else:
                cond_fluent = self._prologify(elem['name'])
                is_functional, branch_values = self._analyze_gateway_branches(elem_id, process_data)
                
                if is_functional:
                    # Multi-branch gateway with functional fluent
                    # Generate nested if statements: if(fluent(ID) = value1, proc1, if(fluent(ID) = value2, proc2, default_proc))
                    default_flow_id = elem.get('default')
                    default_proc = None
                    non_default_branches = []
                    
                    for flow_id in outgoing_flows:
                        target_id = process_data['sequence_flows'][flow_id]['target']
                        branch_proc = self._build_proc_for_element(target_id, process_data, visited.copy())
                        branch_value = branch_values.get(flow_id, 'unknown')
                        
                        if flow_id == default_flow_id:
                            # Default branch - will be the final else
                            default_proc = branch_proc
                        else:
                            # Named branch with specific value condition
                            non_default_branches.append((branch_value, branch_proc))
                    
                    # Build nested if statements from the branches
                    # Start with the default (or [] if no default)
                    result = default_proc if default_proc else "[]"
                    
                    # Wrap each non-default branch in an if, working backwards
                    for branch_value, branch_proc in reversed(non_default_branches):
                        result = f"if({cond_fluent}(ID) = {branch_value}, {branch_proc}, {result})"
                    
                    return result
                else:
                    # Binary gateway (traditional true/false)
                    default_flow_id = elem.get('default')
                    then_flow_id = next((f for f in outgoing_flows if f != default_flow_id), None)
                    then_proc = self._build_proc_for_element(process_data['sequence_flows'][then_flow_id]['target'], process_data, visited.copy()) if then_flow_id else "[]"
                    else_proc = self._build_proc_for_element(process_data['sequence_flows'][default_flow_id]['target'], process_data, visited.copy()) if default_flow_id else "[]"
                    return f"if({cond_fluent}(ID), {then_proc}, {else_proc})"
        elif self._is_element_type(elem, EVENT_BASED_GATEWAY):
            branches = []
            for flow_id in outgoing_flows:
                target_id = process_data['sequence_flows'][flow_id]['target']
                catch_event = process_data['elements'][target_id]
                source_event = self._find_throw_event_for_catch(target_id)
                wait_event_name = self._prologify(source_event['name'])
                rest_of_branch_id = process_data['sequence_flows'][catch_event['outgoing'][0]]['target']
                rest_of_branch_proc = self._build_proc_for_element(rest_of_branch_id, process_data, visited.copy())
                branches.append(f"[?(done({wait_event_name}(ID))), {rest_of_branch_proc}]")
            return "ndet(" + ", ".join(branches) + ")"
        elif self._is_element_type(elem, PARALLEL_GATEWAY):
            join_gateway_id = self._find_join_gateway(elem_id, process_data)
            branch_starts, branch_terminators = [], []
            
            for flow_id in outgoing_flows:
                branch_start_id = process_data['sequence_flows'][flow_id]['target']
                branch_start_elem = process_data['elements'][branch_start_id]
                if self._is_element_type(branch_start_elem, TASK):
                    branch_start_name = self._prologify(branch_start_elem['name'])
                    branch_starts.append(f"{branch_start_name}(start, ID)")
                    branch_terminators.append(f"done({branch_start_name}(end, ID))")
            
            if len(branch_terminators) > 1:
                wait_proc = f"?(and({', '.join(sorted(branch_terminators))}))"
            elif branch_terminators:
                wait_proc = f"?({branch_terminators[0]})"
            else:
                wait_proc = ""
            
            join_elem = process_data['elements'][join_gateway_id]
            if join_elem['outgoing']:
                next_elem_id = process_data['sequence_flows'][join_elem['outgoing'][0]]['target']
                next_proc = self._build_proc_for_element(next_elem_id, process_data, visited)
            else:
                next_proc = "[]"
            
            conc_part = f"conc({', '.join(branch_starts)})" if len(branch_starts) > 1 else branch_starts[0] if branch_starts else "[]"
            
            if wait_proc and next_proc != "[]":
                return f"[{conc_part}, {wait_proc}, {next_proc}]"
            elif wait_proc:
                return f"[{conc_part}, {wait_proc}]"
            else:
                return conc_part
        
        next_elem_id = process_data['sequence_flows'][outgoing_flows[0]]['target']
        next_proc = self._build_proc_for_element(next_elem_id, process_data, visited)
        
        if not current_proc:
            return next_proc
        if next_proc == "[]":
            return current_proc
        
        current_is_list = current_proc.startswith('[') and not current_proc.startswith('[?')
        next_is_list = next_proc.startswith('[') and not next_proc.startswith('[?')
        
        if current_is_list and next_is_list:
            return f"[{current_proc[1:-1]}, {next_proc[1:-1]}]"
        elif current_is_list:
            return f"[{current_proc[1:-1]}, {next_proc}]"
        elif next_is_list:
            return f"[{current_proc}, {next_proc[1:-1]}]"
        else:
            return f"[{current_proc}, {next_proc}]"

    def _get_all_path_conditions(self, start_node_id, process_data):
        """
        Enhanced: If an activity is after a parallel gateway, and the parallel is immediately preceded by another gateway (with no task in between),
        look before that gateway. If the other gateway is exclusive, use OR of done() of all tasks before it; if parallel, use AND.
        """
        paths = []
        q = [(start_node_id, [])]
        max_depth = 20
        while q and max_depth > 0:
            max_depth -= 1
            current_id, current_path_conditions = q.pop(0)
            elem = process_data['elements'].get(current_id)
            if not elem:
                continue

            predecessors = self._find_true_predecessors(current_id, process_data)
            if predecessors:
                # Check if these predecessors come from a gateway
                immediate_sources = []
                for inc_id in elem.get('incoming', []):
                    source_id = process_data['sequence_flows'][inc_id]['source']
                    immediate_sources.append(source_id)

                # If all flows come from the same gateway, check its type
                if len(set(immediate_sources)) == 1:
                    gateway = process_data['elements'].get(immediate_sources[0])
                    if gateway and GATEWAY in gateway['type'].lower():
                        # Check if this is a DIVERGING gateway (split) - we need to look BEFORE the converging gateway
                        # A diverging parallel gateway has 1 incoming and multiple outgoing
                        is_parallel_diverging = (self._is_element_type(gateway, PARALLEL_GATEWAY) and 
                                                len(gateway.get('incoming', [])) == 1 and 
                                                len(gateway.get('outgoing', [])) > 1)
                        
                        if is_parallel_diverging:
                            # This is the case you described: activity after parallel split
                            # Need to look BEFORE this parallel to see if there's another gateway
                            gateway_incoming_flows = gateway.get('incoming', [])
                            if gateway_incoming_flows:
                                before_parallel_id = process_data['sequence_flows'][gateway_incoming_flows[0]]['source']
                                before_parallel_elem = process_data['elements'].get(before_parallel_id)
                                
                                # Check if the element before the parallel is a CONVERGING gateway
                                if before_parallel_elem and GATEWAY in before_parallel_elem['type'].lower():
                                    is_converging = len(before_parallel_elem.get('incoming', [])) > 1 and len(before_parallel_elem.get('outgoing', [])) == 1
                                    
                                    if is_converging:
                                        # Look before this converging gateway to find the tasks
                                        before_converging_preds = self._find_true_predecessors(before_parallel_elem['id'], process_data)
                                        
                                        if self._is_element_type(before_parallel_elem, EXCLUSIVE_GATEWAY):
                                            # OR of done() of all tasks before the exclusive converging gateway
                                            or_conds = []
                                            for p in before_converging_preds:
                                                pred_cond = self._get_done_condition_for_element(p)
                                                if pred_cond:
                                                    or_conds.append(pred_cond)
                                            if or_conds:
                                                paths.append(current_path_conditions + [self._build_nested_or(sorted(or_conds))])
                                                continue
                                        elif self._is_element_type(before_parallel_elem, PARALLEL_GATEWAY):
                                            # AND of done() of all tasks before the parallel converging gateway
                                            and_conds = []
                                            for p in before_converging_preds:
                                                pred_cond = self._get_done_condition_for_element(p)
                                                if pred_cond:
                                                    and_conds.append(pred_cond)
                                            if and_conds:
                                                paths.append(current_path_conditions + [self._build_nested_and(sorted(and_conds))])
                                                continue
                # Fallback to original logic
                # If multiple predecessors and it's NOT a parallel join, it's an XOR merge
                is_parallel_join = False
                if len(set(immediate_sources)) == 1:
                    gateway = process_data['elements'].get(immediate_sources[0])
                    if gateway and 'parallelGateway' in gateway['type']:
                        is_parallel_join = True
                is_xor_merge = len(predecessors) > 1 and not is_parallel_join
                if is_xor_merge:
                    for p in predecessors:
                        pred_cond = self._get_done_condition_for_element(p)
                        if pred_cond:
                            paths.append(current_path_conditions + [pred_cond])
                else:
                    pred_conds = []
                    for p in predecessors:
                        pred_cond = self._get_done_condition_for_element(p)
                        if pred_cond:
                            pred_conds.append(pred_cond)
                    paths.append(current_path_conditions + pred_conds)
                continue

            if not elem.get('incoming'):
                if current_path_conditions:
                    paths.append(current_path_conditions)
                continue

            for inc_flow_id in elem.get('incoming'):
                source_id = process_data['sequence_flows'][inc_flow_id]['source']
                source_elem = process_data['elements'].get(source_id)
                if not source_elem:
                    continue
                new_path_conditions = list(current_path_conditions)
                if self._is_element_type(source_elem, EXCLUSIVE_GATEWAY) and source_elem['name']:
                    fluent_name = self._prologify(source_elem['name'])
                    is_functional, branch_values = self._analyze_gateway_branches(source_elem['id'], process_data)
                    if is_functional:
                        branch_value = branch_values.get(inc_flow_id, 'unknown')
                        new_path_conditions.append(f"{fluent_name}(ID) = {branch_value}")
                    else:
                        fluent = fluent_name + "(ID)"
                        is_default = source_elem.get('default') == inc_flow_id
                        new_path_conditions.append(f"neg({fluent})" if is_default else fluent)
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
                
                if self._is_element_type(source_elem, EXCLUSIVE_GATEWAY) and source_elem['name']:
                    pass
                elif GATEWAY in source_elem['type'].lower(): 
                    q.append(source_id)
                elif self._is_element_type(source_elem, INTERMEDIATE_CATCH_EVENT):
                    throw_elem = self._find_throw_event_for_catch(source_id)
                    if throw_elem:
                        predecessors.append(throw_elem)
                else: 
                    predecessors.append(source_elem)
        return predecessors

    def _find_join_gateway(self, split_gateway_id, process_data):
        q = [process_data['sequence_flows'][f]['target'] for f in process_data['elements'][split_gateway_id]['outgoing']]
        visited = set(q)
        while q:
            curr_id = q.pop(0)
            curr_elem = process_data['elements'].get(curr_id)
            if curr_elem and self._is_element_type(curr_elem, PARALLEL_GATEWAY) and len(curr_elem['incoming']) > 1:
                return curr_id
            if curr_elem:
                for f in curr_elem['outgoing']:
                    next_id = process_data['sequence_flows'][f]['target']
                    if next_id not in visited:
                        q.append(next_id)
                        visited.add(next_id)
        return None

    def _generate_initial_state(self, fluents):
        return INITIAL_SITUATION

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
            if SUBPROCESS in e['type'] and elem_id in e.get('contained_elements', []):
                return True
        return False

    def _find_exception_completion_marker(self, proc_id):
        """
        Find the completion marker for the exception handler (event subprocess) in a process.
        Returns the marker element (throw event or end event) that signals exception handling is complete.
        """
        process_data = self.parser.processes.get(proc_id)
        if not process_data:
            return None
        
        # Find event subprocesses in this process
        for elem in process_data['elements'].values():
            if elem.get('is_event_subprocess'):
                # Found an event subprocess, find its completion marker
                return self._find_subprocess_completion_marker(elem['id'], process_data)
        
        return None
    
    def _find_exception_trigger_event(self, proc_id):
        """
        Find the exception trigger event (start event of event subprocess) in a process.
        This is the exogenous action that triggers the exception handler.
        For message-based exceptions, traces back to the original trigger in the source pool.
        Returns the start event element of the event subprocess.
        """
        process_data = self.parser.processes.get(proc_id)
        if not process_data:
            return None
        
        # Find event subprocesses in this process
        for elem in process_data['elements'].values():
            if elem.get('is_event_subprocess'):
                # Found an event subprocess, find its start event (the trigger)
                start_event = self._find_subprocess_start_event(elem['id'], process_data)
                if not start_event:
                    continue
                
                # Check if this is a message catch start event (receives a message)
                is_message_catch = any(v == start_event['id'] for v in self.parser.message_flows.values())
                
                if is_message_catch:
                    # This is a message catch - find the original exception trigger
                    # Find the message flow where this event is the target
                    for source_id, target_id in self.parser.message_flows.items():
                        if target_id == start_event['id']:
                            # Found the message flow, now find the SOURCE pool's exception trigger
                            # The source is an intermediate throw, we need its pool's event subprocess start
                            for other_proc_id, other_proc_data in self.parser.processes.items():
                                if source_id in other_proc_data['elements']:
                                    # Recursively find the exception trigger in the source pool
                                    return self._find_exception_trigger_event(other_proc_id)
                            break
                else:
                    # Direct exception trigger (not via message) - this is the root!
                    return start_event
        
        return None
    
    def _generate_exception_trigger_precondition(self, start_event_id, event_name, proc_id, process_data):
        """
        Generate the precondition for an exception trigger (event subprocess start event).
        
        The precondition must ensure:
        1. Valid ID and pool
        2. None of the end events in participating pools have been reached
        3. Not waiting (already acquired)
        4. Active in all participating pools
        5. Exception hasn't already been triggered
        
        Args:
            start_event_id: ID of the exception start event
            event_name: Prologified name of the event
            proc_id: Process ID containing this exception
            process_data: Process data dictionary
            
        Returns:
            String with the Prolog precondition
        """
        conditions = []
        
        # Find all pools involved (where this exception can propagate via message flows)
        involved_pools = set()
        pool_obj = self._find_pool_for_element(start_event_id)
        if pool_obj:
            current_pool_name = self._prologify(pool_obj['name'])
            involved_pools.add(current_pool_name)
            
            # Find other pools connected via message flows from this exception handler
            subprocess_elem = self._find_parent_subprocess(start_event_id, process_data)
            if subprocess_elem:
                for contained_id in subprocess_elem.get('contained_elements', []):
                    if contained_id in self.parser.message_flows:
                        target_id = self.parser.message_flows[contained_id]
                        target_pool_obj = self._find_pool_for_element(target_id)
                        if target_pool_obj:
                            target_pool_name = self._prologify(target_pool_obj['name'])
                            involved_pools.add(target_pool_name)
        
        # Find all end events in all pools
        end_event_names = []
        for p_id, p_data in self.parser.processes.items():
            for elem_id, elem in p_data['elements'].items():
                if 'endEvent' in elem['type'] and not self._is_in_subprocess(elem_id, p_data):
                    # Regular end event (not in exception handler)
                    end_name = self._prologify(elem['name'])
                    if end_name:
                        end_event_names.append(f"neg(done({end_name}(ID)))")
        
        # Build the complex precondition
        # Pattern: and(pool(POOL), and(neg(done(end1)), and(neg(done(end2)), and(...))))
        
        # Start with pool variable quantification
        conditions.append("pool(POOL)")
        
        # Add negations of all end events
        for end_condition in sorted(end_event_names):
            conditions.append(end_condition)
        
        # Add: not waiting in any pool
        conditions.append("neg(waiting(ID, POOL))")
        
        # Add: active in the variable pool (covers any pool where the exception can trigger)
        # This means the instance must be active in at least one pool, not all pools
        conditions.append("active(ID, POOL)")
        
        # Add: exception not already triggered
        conditions.append(f"neg(done({event_name}(ID)))")
        
        # Build nested and() structure
        if len(conditions) == 0:
            return f"neg(done({event_name}(ID)))"
        elif len(conditions) == 1:
            return conditions[0]
        else:
            # Build right-nested and() structure: and(A, and(B, and(C, D)))
            result = conditions[-1]
            for i in range(len(conditions) - 2, -1, -1):
                result = f"and({conditions[i]}, {result})"
            return result

    def _find_subprocess_start_event(self, subprocess_id, process_data):
        sub_proc = process_data['elements'].get(subprocess_id)
        if not sub_proc or 'contained_elements' not in sub_proc: return None
        for contained_id in sub_proc['contained_elements']:
            elem = process_data['elements'].get(contained_id)
            if elem and 'startEvent' in elem['type']: return elem
        return None
    
    def _find_parent_subprocess(self, elem_id, process_data):
        """Find the subprocess that contains the given element."""
        for elem in process_data['elements'].values():
            if 'subProcess' in elem.get('type', '') or elem.get('is_event_subprocess'):
                if 'contained_elements' in elem and elem_id in elem['contained_elements']:
                    return elem
        return None
    
    def _find_subprocess_completion_marker(self, subprocess_id, process_data):
        """
        Find the completion marker for an event subprocess.
        This could be a message throw event or the final event in the subprocess.
        """
        sub_proc = process_data['elements'].get(subprocess_id)
        if not sub_proc or 'contained_elements' not in sub_proc: return None
        
        # First, look for intermediate throw events (like "withdrawal sent")
        for contained_id in sub_proc['contained_elements']:
            elem = process_data['elements'].get(contained_id)
            if elem and 'intermediateThrowEvent' in elem['type'] and elem.get('name'):
                # Check if this throw event is part of a message flow
                if contained_id in self.parser.message_flows:
                    return elem
        
        # If no throw event found, look for the end event
        for contained_id in sub_proc['contained_elements']:
            elem = process_data['elements'].get(contained_id)
            if elem and 'endEvent' in elem['type'] and elem.get('name'):
                return elem
        
        return None
    
    def _is_decision_task(self, task_id, process_data):
        task = process_data['elements'].get(task_id)
        if not task or not task['outgoing']: return False
        next_elem_id = process_data['sequence_flows'][task['outgoing'][0]]['target']
        next_elem = process_data['elements'].get(next_elem_id)
        return next_elem and 'exclusiveGateway' in next_elem['type'] and next_elem['name']

    def _get_decision_values(self, task_id, process_data):
        """
        Get the possible decision values for a decision task.
        Returns (is_functional, values_list) where:
        - is_functional: True if the gateway uses functional fluent, False for boolean
        - values_list: List of possible Prolog atom values
        """
        task = process_data['elements'].get(task_id)
        if not task or not task['outgoing']: 
            return False, []
        
        next_elem_id = process_data['sequence_flows'][task['outgoing'][0]]['target']
        next_elem = process_data['elements'].get(next_elem_id)
        
        if not (next_elem and 'exclusiveGateway' in next_elem['type'] and next_elem['name']):
            return False, []
        
        # Use existing gateway analysis
        is_functional, branch_values_dict = self._analyze_gateway_branches(next_elem['id'], process_data)
        
        if is_functional:
            # Return the Prologified branch values (excluding 'default' if present)
            values = [v for v in branch_values_dict.values() if v != 'default']
            return True, values
        else:
            # Boolean decision
            return False, ['true', 'false']

    def _generate_footer(self):
        return FOOTER