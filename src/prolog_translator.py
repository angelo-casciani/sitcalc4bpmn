import re
from prolog_templates import *
from prolog_formatter import format_proc_clause

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
        # Track loop-controlling fluents: {fluent_name: loop_continuing_value}
        self.loop_fluents = {}
        # Track ALL gateway fluents: {fluent_name: default_value}
        self.gateway_fluents = {}
        # Track functional (non-boolean) gateway fluents
        self.functional_fluents = set()

    def _prologify(self, name):
        if not name: return ""
        s = name.lower()
        s = s.replace('>', 'larger')
        s = s.replace('<', 'smaller')
        s = s.replace('=', 'equal')
        s = s.replace('+', 'and')
        s = s.replace('ß', 'ss')
        s = s.replace('ä', 'a')
        s = s.replace('ö', 'o')
        s = s.replace('ü', 'u')
        s = s.strip(' ')
        s = re.sub(r'[\s-]+', '_', s)
        s = re.sub(r'[?\'\"!@#$%^*+~`|\\/:;&\(\)\[\]{},.]', '', s)
        if s and not s[0].islower():
            if s[0].isdigit():
                s = 'n' + s  # 'n' for numeric
            elif s[0] == '_':
                s = 'v' + s  # 'v' for value
            else:
                s = 'x' + s  # 'x' for other
        return s
    
    def _get_or_generate_gateway_fluent_name(self, gateway_elem, process_data):
        """Get the fluent name for a gateway, generating one if the gateway has no name."""
        if gateway_elem.get('name'):
            return self._prologify(gateway_elem['name'])
        
        # Generate a name based on the gateway's outgoing flow labels
        outgoing_flows = gateway_elem.get('outgoing', [])
        flow_labels = []
        for flow_id in outgoing_flows:
            flow_info = process_data['sequence_flows'].get(flow_id, {})
            label = flow_info.get('name', '').strip()
            if label:
                flow_labels.append(self._prologify(label))
        
        # Use the flow labels to create a descriptive name
        if len(flow_labels) >= 2:
            # Use first two labels: "choice_label1_or_label2"
            return f"choice_{flow_labels[0]}_or_{flow_labels[1]}"
        elif len(flow_labels) == 1:
            # Use single label: "choice_label"
            return f"choice_{flow_labels[0]}"
        else:
            # Last resort: use gateway ID suffix
            gw_id = gateway_elem.get('id', '')
            suffix = gw_id[-8:] if len(gw_id) > 8 else gw_id
            return f"gateway_{suffix}"
    
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
        """Generate the done() condition for a predecessor element.
        For decision tasks (those with 3-parameter end actions), uses existential quantification
        over the result parameter: some(res, done(task(end, ID, res)))"""
        if self._is_element_type(elem, TASK):
            task_name = self._prologify(elem['name'])
            # Check if this is a decision task by finding its process context
            # We need to check across all processes since elem might not have process context
            for proc_id, process in self.parser.processes.items():
                if elem['id'] in process['elements']:
                    if self._is_decision_task(elem['id'], process):
                        # Decision task: use existential quantification over the result
                        return f"some(res, done({task_name}(end, ID, res)))"
                    break
            # Regular task: 2 parameters
            return f"done({task_name}(end, ID))"
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

    def _build_nested_conc(self, processes):
        """Build nested binary conc/2 predicates from a list of concurrent processes."""
        if not processes:
            return "[]"
        if len(processes) == 1:
            return processes[0]
        if len(processes) == 2:
            return f"conc({processes[0]}, {processes[1]})"
        # For more than 2, nest recursively: conc(P1, conc(P2, conc(P3, ...)))
        return f"conc({processes[0]}, {self._build_nested_conc(processes[1:])})"

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
        # Pre-pass: detect loop structures to identify loop-controlling fluents
        # This must happen before generating fluents/causes so initially clauses can be added
        self._detect_all_loops()
        # Pre-pass: detect ALL gateways to identify gateway fluents needing initialization
        self._detect_all_gateways()
        
        code = self._generate_header()
        code += self._generate_fluents_and_causes()
        code += self._generate_actions_and_preconditions()
        code += self._generate_procedures()
        code += self._generate_footer()
        return code

    def _detect_all_loops(self):
        """Pre-pass to detect all loop structures and identify loop-controlling fluents.
        This populates self.loop_fluents before code generation begins."""
        for proc_id, process in self.parser.processes.items():
            for elem_id, elem in process['elements'].items():
                if not self._is_element_type(elem, EXCLUSIVE_GATEWAY):
                    continue
                
                incoming_flows = elem.get('incoming', [])
                outgoing_flows = elem.get('outgoing', [])
                is_merge = len(incoming_flows) > 1 and len(outgoing_flows) == 1
                
                if not is_merge:
                    continue
                
                # Check if this is part of a loop structure
                is_loop, div_gw_id, loop_back_flow, exit_flow = self._detect_loop_structure(elem_id, process)
                
                if not is_loop:
                    continue
                
                # Found a loop! Extract the loop-controlling fluent and value
                div_gw = process['elements'][div_gw_id]
                is_functional, branch_values = self._analyze_gateway_branches(div_gw_id, process)
                
                # Determine which flows loop back and which exit
                div_gw_outgoing = div_gw.get('outgoing', [])
                loop_back_flow_id = None
                
                for flow_id in div_gw_outgoing:
                    if self._flow_reaches_target(flow_id, elem_id, process, max_depth=50):
                        loop_back_flow_id = flow_id
                        break
                
                if not loop_back_flow_id:
                    continue
                
                # Get the fluent name
                cond_fluent = self._get_or_generate_gateway_fluent_name(div_gw, process)
                
                # Determine the loop-continuing value
                if is_functional:
                    loop_value = branch_values.get(loop_back_flow_id, 'branch_1')
                    self.loop_fluents[cond_fluent] = loop_value
                else:
                    default_flow = div_gw.get('default')
                    if loop_back_flow_id == default_flow:
                        self.loop_fluents[cond_fluent] = 'false'
                    else:
                        self.loop_fluents[cond_fluent] = 'true'

    def _detect_all_gateways(self):
        """Pre-pass to detect all exclusive gateways and their default values.
        This populates self.gateway_fluents before code generation begins."""
        for proc_id, process in self.parser.processes.items():
            for elem_id, elem in process['elements'].items():
                if not self._is_element_type(elem, EXCLUSIVE_GATEWAY):
                    continue
                
                outgoing_flows = elem.get('outgoing', [])
                
                # Only process diverging gateways (splits)
                if len(outgoing_flows) <= 1:
                    continue
                
                # Get the fluent name
                cond_fluent = self._get_or_generate_gateway_fluent_name(elem, process)
                
                # Skip if already tracked as a loop fluent (loop value takes precedence)
                if cond_fluent in self.loop_fluents:
                    continue
                
                # Analyze gateway branches to determine default value
                is_functional, branch_values = self._analyze_gateway_branches(elem_id, process)
                
                if is_functional:
                    # For functional gateways, use the first branch value
                    if outgoing_flows and outgoing_flows[0] in branch_values:
                        default_value = branch_values[outgoing_flows[0]]
                        self.gateway_fluents[cond_fluent] = default_value
                else:
                    # For boolean gateways, use default flow if specified, otherwise 'true'
                    default_flow = elem.get('default')
                    if default_flow:
                        self.gateway_fluents[cond_fluent] = 'false'
                    else:
                        self.gateway_fluents[cond_fluent] = 'true'
    
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

    def _detect_loop_structure(self, converging_gw_id, process_data):
        """
        Detect if a converging exclusive gateway is part of a loop structure.
        Returns: (is_loop, diverging_gw_id, loop_back_flow_id, exit_flow_id)
        
        Loop pattern:
        - Converging gateway with multiple incoming flows
        - Followed by tasks/events and then a diverging gateway (can have elements in between)
        - One branch from diverging gateway loops back to converging gateway
        - Other branch(es) exit the loop
        """
        converging_gw = process_data['elements'].get(converging_gw_id)
        if not converging_gw or not self._is_element_type(converging_gw, EXCLUSIVE_GATEWAY):
            return False, None, None, None
        
        incoming_flows = converging_gw.get('incoming', [])
        outgoing_flows = converging_gw.get('outgoing', [])
        
        # Must be a merge (multiple incoming, single outgoing)
        if len(incoming_flows) <= 1 or len(outgoing_flows) != 1:
            return False, None, None, None
        
        # Find the next diverging gateway (can have tasks/events in between)
        next_elem_id = process_data['sequence_flows'][outgoing_flows[0]]['target']
        diverging_gw_id = self._find_next_diverging_gateway(next_elem_id, process_data, max_depth=20)
        
        if not diverging_gw_id:
            return False, None, None, None
        
        next_elem = process_data['elements'].get(diverging_gw_id)
        
        diverging_outgoing = next_elem.get('outgoing', [])
        if len(diverging_outgoing) < 2:
            return False, None, None, None
        
        # Check if any branch loops back to the converging gateway
        loop_back_flows = []
        exit_flows = []
        
        for flow_id in diverging_outgoing:
            # Trace this flow to see if it eventually reaches the converging gateway
            if self._flow_reaches_target(flow_id, converging_gw_id, process_data, max_depth=50):
                loop_back_flows.append(flow_id)
            else:
                exit_flows.append(flow_id)
        
        # If both branches from the immediate diverging gateway loop back,
        # find the actual decision gateway deeper in the flow
        if loop_back_flows and not exit_flows:
            # Both branches loop back - need to find the gateway that decides to exit
            decision_gw = self._find_loop_decision_gateway(next_elem_id, converging_gw_id, process_data)
            if decision_gw:
                return True, decision_gw, None, None  # We'll determine loop/exit flows later
        
        # Accept either pattern:
        # 1. One branch loops back, others exit (standard while loop)
        if exit_flows and loop_back_flows:
            # This is a loop structure
            # Use the first loop-back flow (they should all loop back anyway in pattern 2)
            return True, diverging_gw_id, loop_back_flows[0], exit_flows[0]
        
        return False, None, None, None
    
    def _find_next_diverging_gateway(self, start_elem_id, process_data, max_depth=20):
        """
        Find the next diverging exclusive gateway following a path from start_elem_id.
        Skips over tasks and events to find a gateway with multiple outgoing flows.
        Returns the gateway ID or None if not found.
        """
        current_id = start_elem_id
        depth = 0
        
        while current_id and depth < max_depth:
            elem = process_data['elements'].get(current_id)
            if not elem:
                return None
            
            # Check if this is a diverging exclusive gateway
            if self._is_element_type(elem, EXCLUSIVE_GATEWAY):
                outgoing = elem.get('outgoing', [])
                if len(outgoing) >= 2:
                    return current_id
            
            # Move to next element (follow single outgoing flow)
            outgoing = elem.get('outgoing', [])
            if len(outgoing) != 1:
                # Either no outgoing or multiple outgoing - stop here
                return None
            
            flow_id = outgoing[0]
            flow_info = process_data['sequence_flows'].get(flow_id)
            if not flow_info:
                return None
            
            current_id = flow_info['target']
            depth += 1
        
        return None
    
    def _find_loop_decision_gateway(self, start_elem_id, loop_target_id, process_data, max_depth=50):
        """
        Find the gateway that actually decides whether to loop back or exit.
        This is the gateway where one branch reaches loop_target and another doesn't.
        """
        visited = set()
        queue = [(start_elem_id, [])]  # (elem_id, path)
        
        while queue and max_depth > 0:
            max_depth -= 1
            elem_id, path = queue.pop(0)
            
            if elem_id in visited:
                continue
            visited.add(elem_id)
            
            elem = process_data['elements'].get(elem_id)
            if not elem:
                continue
            
            # Check if this is an exclusive gateway with diverging branches
            if self._is_element_type(elem, EXCLUSIVE_GATEWAY):
                outgoing = elem.get('outgoing', [])
                if len(outgoing) >= 2:
                    # Check if one branch loops back and another doesn't
                    loops_back = []
                    exits = []
                    
                    for flow_id in outgoing:
                        if self._flow_reaches_target(flow_id, loop_target_id, process_data, max_depth=30):
                            loops_back.append(flow_id)
                        else:
                            exits.append(flow_id)
                    
                    # This is the decision gateway if it has both loop-back and exit branches
                    if loops_back and exits:
                        return elem_id
            
            # Continue searching through outgoing flows
            for out_flow_id in elem.get('outgoing', []):
                next_flow = process_data['sequence_flows'].get(out_flow_id)
                if next_flow and next_flow['target'] not in visited:
                    queue.append((next_flow['target'], path + [elem_id]))
        
        return None
    
    def _flow_reaches_target(self, flow_id, target_elem_id, process_data, max_depth=50):
        """Check if following a flow eventually reaches a target element."""
        if max_depth <= 0:
            return False
        
        flow_info = process_data['sequence_flows'].get(flow_id)
        if not flow_info:
            return False
        
        current_elem_id = flow_info['target']
        if current_elem_id == target_elem_id:
            return True
        
        # BFS to find if we can reach target
        visited = set()
        queue = [current_elem_id]
        
        while queue and max_depth > 0:
            max_depth -= 1
            elem_id = queue.pop(0)
            
            if elem_id in visited:
                continue
            visited.add(elem_id)
            
            if elem_id == target_elem_id:
                return True
            
            elem = process_data['elements'].get(elem_id)
            if not elem:
                continue
            
            # Add outgoing elements to queue
            for out_flow_id in elem.get('outgoing', []):
                next_flow = process_data['sequence_flows'].get(out_flow_id)
                if next_flow:
                    queue.append(next_flow['target'])
        
        return False

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
        all_branches_labeled = True
        
        labeled_branches = {}
        for flow_id in outgoing_flows:
            flow_info = process_data['sequence_flows'].get(flow_id, {})
            flow_label = flow_info.get('name', '').strip()
            
            if flow_id == default_flow:
                # Default branch - will assign value later based on labeled branches
                branch_values[flow_id] = 'default'
            elif flow_label:
                # Non-empty label - create a Prolog atom from it
                labeled_count += 1
                prologified = self._prologify(flow_label)
                branch_values[flow_id] = prologified
                labeled_branches[flow_id] = prologified
                
                # Check if this is a non-boolean label (not "yes", "no", "true", "false")
                label_lower = flow_label.lower().strip()
                if label_lower not in ['yes', 'no', 'true', 'false', '']:
                    has_non_boolean_labels = True
            else:
                # Empty label - use flow index or generate default name
                all_branches_labeled = False
                branch_values[flow_id] = f'branch_{len(branch_values) + 1}'
        
        # For functional fluents with a default branch, generate a meaningful default value
        # based on the labeled branch (e.g., "familie_vorhanden" -> default is "keine_familie")
        if default_flow and len(labeled_branches) == 1 and has_non_boolean_labels:
            labeled_value = list(labeled_branches.values())[0]
            # Generate a complementary value for the default branch
            if labeled_value.startswith('ohne_'):
                default_value = f"mit_{labeled_value[5:]}"
            elif labeled_value.startswith('mit_'):
                default_value = f"ohne_{labeled_value[4:]}"
            elif labeled_value.startswith('nicht_'):
                default_value = labeled_value[6:]  # Remove 'nicht_' prefix
            elif '_vorhanden' in labeled_value:
                default_value = labeled_value.replace('_vorhanden', '_nicht_vorhanden')
            else:
                # Generic: add 'not_' prefix or similar
                default_value = f"not_{labeled_value}"
            branch_values[default_flow] = default_value
        
        # Use functional fluent if:
        # 1. More than 2 branches with distinct labels, OR
        # 2. Exactly 2 branches but with explicit non-boolean labels (e.g., "stay" vs "go back"), OR
        # 3. All branches (2+) have labels but no default is specified AND labels are non-boolean (need to treat as functional)
        # Note: If all labels are boolean (true/false), treat as relational fluent, not functional
        all_labels_boolean = all(v in ['true', 'false', 'default'] for v in branch_values.values())
        
        is_functional = (len(outgoing_flows) > 2 and labeled_count >= 2) or \
                       (len(outgoing_flows) == 2 and has_non_boolean_labels and labeled_count >= 1) or \
                       (len(outgoing_flows) >= 2 and labeled_count == len(outgoing_flows) and not default_flow and not all_labels_boolean) or \
                       (len(outgoing_flows) >= 2 and labeled_count >= 1 and not all_labels_boolean)  # Any 2+ branches with at least 1 non-boolean labeled
        
        return is_functional, branch_values

    def _add_data_and_gateway_fluents(self, fluents, causes):
        for proc in self.parser.processes.values():
            for elem in proc['elements'].values():
                if self._is_element_type(elem, EXCLUSIVE_GATEWAY):
                    # Check if this gateway has outgoing flows (it's a diverging gateway)
                    outgoing_flows = elem.get('outgoing', [])
                    if len(outgoing_flows) < 2:
                        continue  # Skip merge gateways
                    
                    # Get or generate fluent name for this gateway
                    fluent_name = self._get_or_generate_gateway_fluent_name(elem, proc)
                    
                    # Analyze if this gateway has decision tasks (needs 3-parameter actions)
                    is_functional, branch_values = self._analyze_gateway_branches(elem['id'], proc)
                    
                    # Check if this gateway has NON-BOOLEAN values (needs functional fluent)
                    # Boolean gateways: values are only "true"/"false" -> use rel_fluent
                    # Non-boolean gateways: values like "bewilligt"/"nicht_bewilligt" -> use fun_fluent
                    predecessors = self._find_true_predecessors(elem['id'], proc)
                    
                    # Try to get decision values from predecessor task
                    decision_task = None
                    decision_values = []
                    if predecessors and self._is_element_type(predecessors[0], TASK):
                        pred_task = predecessors[0]
                        _, decision_values = self._get_decision_values(pred_task['id'], proc)
                        if decision_values:  # Only use if we found actual decision values
                            decision_task = pred_task
                    
                    if decision_task:
                        # Gateway has a predecessor decision task with known values
                        # Check if values are boolean (true/false only)
                        is_boolean = set(decision_values) <= {'true', 'false'}
                        
                        if is_boolean:
                            # Relational fluent for boolean gateways
                            fluents.add(f"{fluent_name}(_ID)")
                            task_end = f"{self._prologify(decision_task['name'])}(end, ID, RESULT)"
                            causes.extend([
                                f"causes_true({task_end}, {fluent_name}(ID), RESULT = true).",
                                f"causes_false({task_end}, {fluent_name}(ID), RESULT = false)."
                            ])
                        else:
                            # Functional fluent for non-boolean gateways
                            fluents.add(f"fun:{fluent_name}(_ID)")
                            self.functional_fluents.add(fluent_name)  # Track for initially clauses
                            task_end = f"{self._prologify(decision_task['name'])}(end, ID, VALUE)"
                            # Constrain VALUE to be member of possible values
                            values_str = '[' + ', '.join(decision_values) + ']'
                            causes.append(f"causes_val({task_end}, {fluent_name}(ID), VALUE, member(VALUE, {values_str})).")
                    else:
                        # Gateway without predecessor task - check branch labels to determine type
                        # If branch values are non-boolean, use functional fluent
                        non_default_values = [v for v in branch_values.values() if v not in ['default', 'branch_1']]
                        has_non_boolean = any(v not in ['true', 'false'] for v in non_default_values)
                        
                        if has_non_boolean and non_default_values:
                            # Functional fluent for data-based gateways with specific values
                            fluents.add(f"fun:{fluent_name}(_ID)")
                            self.functional_fluents.add(fluent_name)
                        else:
                            # Relational fluent for boolean/condition gateways
                            fluents.add(f"{fluent_name}(_ID)")
        
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
        
        # Find tasks that are decision tasks (they have RESULT parameters)
        for proc_id, process in self.parser.processes.items():
            for elem_id, elem in process['elements'].items():
                if self._is_element_type(elem, TASK):
                    # Check if this task is a decision task
                    if self._is_decision_task(elem['id'], process):
                        task_name = self._prologify(elem['name'])
                        decision_task_starts.append(f"{task_name}(start, _)")
        
        # Remove duplicates
        decision_task_starts = sorted(set(decision_task_starts))
        
        code = ABBREVIATIONS_HEADER
        if decision_task_starts:
            decision_tasks_str = ",\n      ".join(decision_task_starts)
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
            # Multiple pools: use nested conc/2 (binary only)
            servers = [f'server_{p}' for p in participant_procs]
            conc_expr = self._build_nested_conc(servers)
            code += f"proc(bpmn_process, {conc_expr}).\n\n\n"

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
            # Format the proc clause for better readability
            formatted_proc = format_proc_clause(f"{p_name}(ID)", main_flow_proc)
            code += formatted_proc + "\n\n"

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
            
            # Format the handle_proc clause for better readability
            formatted_handle = format_proc_clause(f"handle_{p_name}(ID)", handle_proc_body)
            code += formatted_handle + "\n\n"
        
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
            
            # For regular start events (not message catch in main process), 
            # do NOT wait for the exogenous start event - it's handled by the acquire mechanism
            # The start event trigger (like job_needed) causes waiting() fluent, which is checked by acquire
            # Only wait for start events if they're in a subprocess
            is_in_subprocess = self._is_in_subprocess(elem_id, process_data)
            
            if elem_name and is_in_subprocess:  # Only wait if in subprocess
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
            
            # If start event has no name or is in main process, skip it and go to next element
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
                # Check if this is part of a loop structure
                is_loop, div_gw_id, loop_back_flow, exit_flow = self._detect_loop_structure(elem_id, process_data)
                
                if is_loop:
                    # This is a loop structure: [?(done(start_event)), while(condition, loop_body)]
                    div_gw = process_data['elements'][div_gw_id]
                    is_functional, branch_values = self._analyze_gateway_branches(div_gw_id, process_data)
                    
                    # Determine which flows loop back and which exit
                    div_gw_outgoing = div_gw.get('outgoing', [])
                    loop_back_flow_id = None
                    exit_flow_id = None
                    
                    for flow_id in div_gw_outgoing:
                        if self._flow_reaches_target(flow_id, elem_id, process_data, max_depth=50):
                            loop_back_flow_id = flow_id
                        else:
                            exit_flow_id = flow_id
                    
                    if not loop_back_flow_id or not exit_flow_id:
                        # Couldn't determine flows properly, fall back to regular merge
                        next_elem_id = process_data['sequence_flows'][outgoing_flows[0]]['target']
                        return self._build_proc_for_element(next_elem_id, process_data, visited)
                    
                    # Determine the loop condition based on the decision gateway
                    # The condition should be TRUE to continue looping (follow loop_back branch)
                    cond_fluent = self._get_or_generate_gateway_fluent_name(div_gw, process_data)
                    
                    if is_functional:
                        # Functional fluent: check which branch is the loop-back
                        loop_value = branch_values.get(loop_back_flow_id, 'branch_1')
                        # while(condition, body) where condition continues the loop
                        loop_condition = f"{cond_fluent}(ID) = {loop_value}"
                        # Track this fluent as loop-controlling with its loop-continuing value
                        self.loop_fluents[cond_fluent] = loop_value
                    else:
                        # Binary fluent: determine if loop_back is the "true" or "default" branch
                        default_flow = div_gw.get('default')
                        
                        if loop_back_flow_id == default_flow:
                            # Loop continues on default (negation of condition)
                            loop_condition = f"neg({cond_fluent}(ID))"
                            # Track as binary fluent with false value for loop continuation
                            self.loop_fluents[cond_fluent] = 'false'
                        else:
                            # Loop continues when condition is true
                            loop_condition = f"{cond_fluent}(ID)"
                            # Track as binary fluent with true value for loop continuation
                            self.loop_fluents[cond_fluent] = 'true'
                    
                    # Build the loop body, but stop at the decision gateway (div_gw_id)
                    # We need to build only up to the decision point, then handle branching specially
                    first_div_gw_id = process_data['sequence_flows'][outgoing_flows[0]]['target']
                    loop_body_visited = visited.copy()
                    loop_body_visited.add(elem_id)  # Don't revisit the converging gateway
                    
                    # Build loop body but when we hit the decision gateway, only include the loop-back branch
                    loop_body = self._build_loop_body_until_decision(
                        first_div_gw_id, div_gw_id, loop_back_flow_id, process_data, loop_body_visited
                    )
                    
                    # Get the exit branch (what follows after the loop)
                    exit_start_id = process_data['sequence_flows'][exit_flow_id]['target']
                    exit_proc = self._build_proc_for_element(exit_start_id, process_data, visited.copy())
                    
                    # Construct: while(condition, loop_body) followed by exit_proc
                    while_construct = f"while({loop_condition}, {loop_body})"
                    
                    if exit_proc and exit_proc != "[]":
                        return f"[{while_construct}, {exit_proc}]"
                    else:
                        return while_construct
                else:
                    # Regular merge, just continue to next element
                    next_elem_id = process_data['sequence_flows'][outgoing_flows[0]]['target']
                    return self._build_proc_for_element(next_elem_id, process_data, visited)
            else:
                cond_fluent = self._get_or_generate_gateway_fluent_name(elem, process_data)
                is_functional, branch_values = self._analyze_gateway_branches(elem_id, process_data)
                
                # Check if this is a boolean or non-boolean gateway
                predecessors = self._find_true_predecessors(elem_id, process_data)
                is_boolean = True
                if predecessors and self._is_element_type(predecessors[0], TASK):
                    _, decision_values = self._get_decision_values(predecessors[0]['id'], process_data)
                    # Only treat as boolean if we have actual decision values that are boolean
                    # Empty decision_values means no decision task, fall back to branch analysis
                    if decision_values:
                        is_boolean = set(decision_values) <= {'true', 'false'}
                    else:
                        # No decision values, check branch labels
                        non_default_values = [v for v in branch_values.values() if v not in ['default', 'branch_1']]
                        is_boolean = not any(v not in ['true', 'false'] for v in non_default_values)
                else:
                    # Gateway without predecessor - check branch values
                    non_default_values = [v for v in branch_values.values() if v not in ['default', 'branch_1']]
                    is_boolean = not any(v not in ['true', 'false'] for v in non_default_values)
                
                # Try to find convergence point for optimization
                convergence_id, is_merge_gw = self._find_exclusive_convergence(elem_id, process_data)
                
                if is_boolean:
                    # Relational fluent - use if(fluent(ID), then, else)
                    default_flow_id = elem.get('default')
                    
                    # Determine which flow is "true" and which is "false" based on labels
                    true_flow_id = None
                    false_flow_id = None
                    for flow_id in outgoing_flows:
                        flow_label = process_data['sequence_flows'][flow_id].get('name', '').lower().strip()
                        if flow_label == 'true':
                            true_flow_id = flow_id
                        elif flow_label == 'false':
                            false_flow_id = flow_id
                    
                    # If we have explicit true/false labels, use them
                    if true_flow_id and false_flow_id:
                        then_flow_id = true_flow_id
                        else_flow_id = false_flow_id
                    else:
                        # Fall back to default-based logic
                        then_flow_id = next((f for f in outgoing_flows if f != default_flow_id), None)
                        else_flow_id = default_flow_id
                    
                    if convergence_id:
                        # Build branches only up to convergence, then continue with shared flow
                        then_proc = self._build_proc_until_target(
                            process_data['sequence_flows'][then_flow_id]['target'], 
                            convergence_id, process_data, visited.copy()
                        ) if then_flow_id else "[]"
                        else_proc = self._build_proc_until_target(
                            process_data['sequence_flows'][else_flow_id]['target'], 
                            convergence_id, process_data, visited.copy()
                        ) if else_flow_id else "[]"
                        
                        # Check if both branches are empty (direct to convergence)
                        if then_proc == "[]" and else_proc == "[]":
                            # No branch-specific logic, just continue with shared flow
                            if is_merge_gw:
                                conv_elem = process_data['elements'][convergence_id]
                                if conv_elem.get('outgoing'):
                                    next_id = process_data['sequence_flows'][conv_elem['outgoing'][0]]['target']
                                    return self._build_proc_for_element(next_id, process_data, visited)
                            else:
                                return self._build_proc_for_element(convergence_id, process_data, visited)
                            return "[]"
                        
                        # Build the shared flow after convergence
                        if is_merge_gw:
                            # If convergence is a merge gateway, continue from its outgoing flow
                            conv_elem = process_data['elements'][convergence_id]
                            if conv_elem.get('outgoing'):
                                next_id = process_data['sequence_flows'][conv_elem['outgoing'][0]]['target']
                                shared_proc = self._build_proc_for_element(next_id, process_data, visited)
                            else:
                                shared_proc = "[]"
                        else:
                            # Convergence is a regular element, include it in shared flow
                            shared_proc = self._build_proc_for_element(convergence_id, process_data, visited)
                        
                        # Build if construct
                        if_construct = f"if({cond_fluent}(ID), {then_proc}, {else_proc})"
                        
                        # Return as sequence: [branch_logic, shared_flow]
                        if shared_proc and shared_proc != "[]":
                            return f"[{if_construct}, {shared_proc}]"
                        else:
                            return if_construct
                    else:
                        # No convergence found, use original logic
                        then_proc = self._build_proc_for_element(process_data['sequence_flows'][then_flow_id]['target'], process_data, visited.copy()) if then_flow_id else "[]"
                        else_proc = self._build_proc_for_element(process_data['sequence_flows'][else_flow_id]['target'], process_data, visited.copy()) if else_flow_id else "[]"
                        return f"if({cond_fluent}(ID), {then_proc}, {else_proc})"
                else:
                    # Functional fluent - use if(fluent(ID) = value, ...)
                    default_flow_id = elem.get('default')
                    default_proc = None
                    non_default_branches = []
                    
                    if convergence_id:
                        # Build branches only up to convergence point
                        all_branches_empty = True
                        for flow_id in outgoing_flows:
                            target_id = process_data['sequence_flows'][flow_id]['target']
                            branch_proc = self._build_proc_until_target(target_id, convergence_id, process_data, visited.copy())
                            branch_value = branch_values.get(flow_id, 'unknown')
                            
                            if branch_proc != "[]":
                                all_branches_empty = False
                            
                            if flow_id == default_flow_id:
                                default_proc = branch_proc
                            else:
                                non_default_branches.append((branch_value, branch_proc))
                        
                        # If all branches are empty (go directly to convergence), skip the if-statement
                        if all_branches_empty:
                            if is_merge_gw:
                                conv_elem = process_data['elements'][convergence_id]
                                if conv_elem.get('outgoing'):
                                    next_id = process_data['sequence_flows'][conv_elem['outgoing'][0]]['target']
                                    return self._build_proc_for_element(next_id, process_data, visited)
                            else:
                                return self._build_proc_for_element(convergence_id, process_data, visited)
                            return "[]"
                        
                        # Build nested if statements for branch selection
                        result = default_proc if default_proc else "[]"
                        for branch_value, branch_proc in reversed(non_default_branches):
                            result = f"if({cond_fluent}(ID) = {branch_value}, {branch_proc}, {result})"
                        
                        # Build the shared flow after convergence
                        if is_merge_gw:
                            # If convergence is a merge gateway, continue from its outgoing flow
                            conv_elem = process_data['elements'][convergence_id]
                            if conv_elem.get('outgoing'):
                                next_id = process_data['sequence_flows'][conv_elem['outgoing'][0]]['target']
                                shared_proc = self._build_proc_for_element(next_id, process_data, visited)
                            else:
                                shared_proc = "[]"
                        else:
                            # Convergence is a regular element, include it in shared flow
                            shared_proc = self._build_proc_for_element(convergence_id, process_data, visited)
                        
                        # Return as sequence: [branch_selection_logic, shared_flow]
                        if shared_proc and shared_proc != "[]":
                            return f"[{result}, {shared_proc}]"
                        else:
                            return result
                    else:
                        # No convergence found, use original logic (full duplication)
                        for flow_id in outgoing_flows:
                            target_id = process_data['sequence_flows'][flow_id]['target']
                            branch_proc = self._build_proc_for_element(target_id, process_data, visited.copy())
                            branch_value = branch_values.get(flow_id, 'unknown')
                            
                            if flow_id == default_flow_id:
                                default_proc = branch_proc
                            else:
                                non_default_branches.append((branch_value, branch_proc))
                        
                        # Build nested if statements
                        result = default_proc if default_proc else "[]"
                        for branch_value, branch_proc in reversed(non_default_branches):
                            result = f"if({cond_fluent}(ID) = {branch_value}, {branch_proc}, {result})"
                        
                        return result
        elif self._is_element_type(elem, EVENT_BASED_GATEWAY):
            branches = []
            for flow_id in outgoing_flows:
                target_id = process_data['sequence_flows'][flow_id]['target']
                catch_event = process_data['elements'][target_id]
                source_event = self._find_throw_event_for_catch(target_id)
                if source_event is None:
                    # If no throw event found, use the catch event itself
                    wait_event_name = self._prologify(catch_event.get('name', f'event_{target_id}'))
                else:
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
            
            # Handle case where join gateway is not found
            if join_gateway_id is None:
                # No join gateway found - just run branches in parallel without explicit join
                conc_part = f"conc({', '.join(branch_starts)})" if len(branch_starts) > 1 else branch_starts[0] if branch_starts else "[]"
                if wait_proc:
                    return f"[{conc_part}, {wait_proc}]"
                else:
                    return conc_part
            
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
                    
                    # Check if this is a boolean or non-boolean gateway
                    # Get decision task values to determine
                    predecessors = self._find_true_predecessors(source_elem['id'], process_data)
                    is_boolean = True
                    if predecessors and self._is_element_type(predecessors[0], TASK):
                        _, decision_values = self._get_decision_values(predecessors[0]['id'], process_data)
                        is_boolean = set(decision_values) <= {'true', 'false'}
                    else:
                        # Gateway without predecessor - check branch values
                        non_default_values = [v for v in branch_values.values() if v not in ['default', 'branch_1']]
                        is_boolean = not any(v not in ['true', 'false'] for v in non_default_values)
                    
                    if is_boolean:
                        # Relational fluent - use fluent(ID) or neg(fluent(ID))
                        fluent = fluent_name + "(ID)"
                        is_default = source_elem.get('default') == inc_flow_id
                        new_path_conditions.append(f"neg({fluent})" if is_default else fluent)
                    else:
                        # Functional fluent - use fluent(ID) = value
                        branch_value = branch_values.get(inc_flow_id, 'unknown')
                        new_path_conditions.append(f"{fluent_name}(ID) = {branch_value}")
                    
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

    def _build_loop_body_until_decision(self, start_id, decision_gw_id, loop_back_flow_id, process_data, visited):
        """
        Build loop body from start_id until we reach decision_gw_id.
        The decision gateway itself is NOT included in the loop body - it's handled by the while condition.
        
        Args:
            start_id: Starting element ID
            decision_gw_id: The decision gateway ID that controls loop exit
            loop_back_flow_id: The flow ID that loops back
            process_data: Process data dictionary
            visited: Set of already visited element IDs
            
        Returns:
            Prolog process string for the loop body
        """
        if start_id in visited:
            return "[]"
        
        if start_id == decision_gw_id:
            # Reached the decision gateway - STOP here, don't include it in the loop body
            # The while condition already handles the decision
            return "[]"
        
        # Normal processing
        visited.add(start_id)
        elem = process_data['elements'].get(start_id)
        
        if not elem:
            return "[]"
        
        # Handle EXCLUSIVE GATEWAY (non-decision)
        if self._is_element_type(elem, EXCLUSIVE_GATEWAY):
            outgoing_flows = elem.get('outgoing', [])
            incoming_flows = elem.get('incoming', [])
            is_merge = len(incoming_flows) > 1 and len(outgoing_flows) == 1
            
            if is_merge:
                # Merge gateway - just continue
                next_elem_id = process_data['sequence_flows'][outgoing_flows[0]]['target']
                return self._build_loop_body_until_decision(
                    next_elem_id, decision_gw_id, loop_back_flow_id, process_data, visited
                )
            else:
                # Split gateway - build with convergence optimization
                cond_fluent = self._get_or_generate_gateway_fluent_name(elem, process_data)
                is_functional, branch_values = self._analyze_gateway_branches(start_id, process_data)
                
                # Check if boolean or functional
                predecessors = self._find_true_predecessors(start_id, process_data)
                is_boolean = True
                if predecessors and self._is_element_type(predecessors[0], TASK):
                    _, decision_values = self._get_decision_values(predecessors[0]['id'], process_data)
                    is_boolean = set(decision_values) <= {'true', 'false'}
                else:
                    non_default_values = [v for v in branch_values.values() if v not in ['default', 'branch_1']]
                    is_boolean = not any(v not in ['true', 'false'] for v in non_default_values)
                
                # Find convergence
                convergence_id, is_merge_gw = self._find_exclusive_convergence(start_id, process_data)
                
                if convergence_id:
                    # Build branches up to convergence
                    default_flow_id = elem.get('default')
                    
                    if is_functional:
                        # Functional fluent
                        default_proc = None
                        non_default_branches = []
                        
                        for flow_id in outgoing_flows:
                            target_id = process_data['sequence_flows'][flow_id]['target']
                            branch_proc = self._build_proc_until_target(target_id, convergence_id, process_data, visited.copy())
                            branch_value = branch_values.get(flow_id, 'unknown')
                            
                            if flow_id == default_flow_id:
                                default_proc = branch_proc
                            else:
                                non_default_branches.append((branch_value, branch_proc))
                        
                        result = default_proc if default_proc else "[]"
                        for branch_value, branch_proc in reversed(non_default_branches):
                            result = f"if({cond_fluent}(ID) = {branch_value}, {branch_proc}, {result})"
                        
                        # Build shared flow after convergence
                        if is_merge_gw:
                            conv_elem = process_data['elements'][convergence_id]
                            if conv_elem.get('outgoing'):
                                next_id = process_data['sequence_flows'][conv_elem['outgoing'][0]]['target']
                                shared_proc = self._build_loop_body_until_decision(
                                    next_id, decision_gw_id, loop_back_flow_id, process_data, visited
                                )
                            else:
                                shared_proc = "[]"
                        else:
                            shared_proc = self._build_loop_body_until_decision(
                                convergence_id, decision_gw_id, loop_back_flow_id, process_data, visited
                            )
                        
                        if shared_proc and shared_proc != "[]":
                            return f"[{result}, {shared_proc}]"
                        else:
                            return result
                    else:
                        # Boolean fluent - similar logic
                        then_flow_id = next((f for f in outgoing_flows if f != default_flow_id), None)
                        then_proc = self._build_proc_until_target(
                            process_data['sequence_flows'][then_flow_id]['target'],
                            convergence_id, process_data, visited.copy()
                        ) if then_flow_id else "[]"
                        else_proc = self._build_proc_until_target(
                            process_data['sequence_flows'][default_flow_id]['target'],
                            convergence_id, process_data, visited.copy()
                        ) if default_flow_id else "[]"
                        
                        if_construct = f"if({cond_fluent}(ID), {then_proc}, {else_proc})"
                        
                        # Build shared flow
                        if is_merge_gw:
                            conv_elem = process_data['elements'][convergence_id]
                            if conv_elem.get('outgoing'):
                                next_id = process_data['sequence_flows'][conv_elem['outgoing'][0]]['target']
                                shared_proc = self._build_loop_body_until_decision(
                                    next_id, decision_gw_id, loop_back_flow_id, process_data, visited
                                )
                            else:
                                shared_proc = "[]"
                        else:
                            shared_proc = self._build_loop_body_until_decision(
                                convergence_id, decision_gw_id, loop_back_flow_id, process_data, visited
                            )
                        
                        if shared_proc and shared_proc != "[]":
                            return f"[{if_construct}, {shared_proc}]"
                        else:
                            return if_construct
                else:
                    # No convergence - shouldn't happen in well-formed BPMN
                    # Fall back to building all branches fully
                    return self._build_proc_for_element(start_id, process_data, visited.copy())
        
        # Continue with normal element processing
        elem = process_data['elements'].get(start_id)
        
        if not elem:
            return "[]"
        
        # Build current element
        current_proc = ""
        
        if self._is_element_type(elem, TASK):
            elem_name = self._prologify(elem['name'])
            is_decision = self._is_decision_task(elem['id'], process_data)
            if is_decision:
                wait_action = f"?(some(res, done({elem_name}(end, ID, res))))"
            else:
                wait_action = f"?(done({elem_name}(end, ID)))"
            current_proc = f"[{elem_name}(start, ID), {wait_action}]"
        elif self._is_element_type(elem, INTERMEDIATE_CATCH_EVENT):
            # Handle intermediate catch events (e.g., timers, message catches)
            elem_name = self._prologify(elem['name'])
            # Check if this is a timer event or message catch
            is_timer = any('timerEventDefinition' in str(child) for child in elem.get('children', []))
            if is_timer and elem_name:
                # Timer events have start/end actions
                current_proc = f"[{elem_name}(start, ID), ?(done({elem_name}(end, ID)))]"
            else:
                # Message/signal catch - wait for corresponding throw event
                source_elem = self._find_throw_event_for_catch(start_id)
                if source_elem:
                    source_name = self._prologify(source_elem['name'])
                    current_proc = f"?(done({source_name}(ID)))"
                elif elem_name:
                    # If no throw event found, treat as a task-like event
                    current_proc = f"[{elem_name}(start, ID), ?(done({elem_name}(end, ID)))]"
                else:
                    current_proc = ""
        elif self._is_element_type(elem, INTERMEDIATE_THROW_EVENT):
            # Handle intermediate throw events
            elem_name = self._prologify(elem['name'])
            current_proc = f"{elem_name}(ID)" if elem_name else ""
        elif self._is_element_type(elem, START_EVENT):
            current_proc = ""
        elif self._is_element_type(elem, END_EVENT):
            elem_name = self._prologify(elem.get('name', 'end_event'))
            if not elem_name or elem_name == 'end_event':
                current_proc = f"end_event(ID)"
            else:
                current_proc = f"{elem_name}(ID)"
        elif self._is_element_type(elem, PARALLEL_GATEWAY):
            # Handle parallel gateway splits
            join_gateway_id = self._find_join_gateway(start_id, process_data)
            if join_gateway_id:
                outgoing_flows = elem.get('outgoing', [])
                branch_starts = []
                branch_terminators = []
                
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
                if join_elem.get('outgoing'):
                    next_elem_id = process_data['sequence_flows'][join_elem['outgoing'][0]]['target']
                    next_proc = self._build_loop_body_until_decision(
                        next_elem_id, decision_gw_id, loop_back_flow_id, process_data, visited
                    )
                else:
                    next_proc = "[]"
                
                conc_part = f"conc({', '.join(branch_starts)})" if len(branch_starts) > 1 else branch_starts[0] if branch_starts else "[]"
                
                if wait_proc and next_proc != "[]":
                    return f"[{conc_part}, {wait_proc}, {next_proc}]"
                elif wait_proc:
                    return f"[{conc_part}, {wait_proc}]"
                else:
                    return conc_part if next_proc == "[]" else f"[{conc_part}, {next_proc}]"
        
        # Get outgoing and continue
        outgoing_flows = elem.get('outgoing', [])
        if not outgoing_flows:
            return current_proc or "[]"
        
        # Continue with next element
        next_elem_id = process_data['sequence_flows'][outgoing_flows[0]]['target']
        next_proc = self._build_loop_body_until_decision(
            next_elem_id, decision_gw_id, loop_back_flow_id, process_data, visited
        )
        
        # Combine current with next
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
    
    def _build_proc_until_target(self, start_id, target_id, process_data, visited):
        """
        Build process from start_id up to (but not including) target_id.
        Stops when target_id is reached.
        
        Args:
            start_id: Starting element ID
            target_id: Target element ID to stop at (not included in result)
            process_data: Process data dictionary
            visited: Set of already visited element IDs
            
        Returns:
            Prolog process string representing the path from start to target
        """
        if start_id == target_id:
            return "[]"
        
        if start_id in visited:
            return "[]"
        
        visited.add(start_id)
        elem = process_data['elements'].get(start_id)
        
        if not elem:
            return "[]"
        
        # Build current element's process
        current_proc = ""
        
        if self._is_element_type(elem, TASK):
            elem_name = self._prologify(elem['name'])
            # Check if this is a decision task
            is_decision = self._is_decision_task(elem['id'], process_data)
            if is_decision:
                wait_action = f"?(some(res, done({elem_name}(end, ID, res))))"
            else:
                wait_action = f"?(done({elem_name}(end, ID)))"
            current_proc = f"[{elem_name}(start, ID), {wait_action}]"
        elif self._is_element_type(elem, START_EVENT):
            current_proc = ""  # Start events handled in preconditions
        elif self._is_element_type(elem, END_EVENT):
            elem_name = self._prologify(elem.get('name', 'end_event'))
            if not elem_name or elem_name == 'end_event':
                current_proc = f"end_event(ID)"
            else:
                current_proc = f"{elem_name}(ID)"
        
        # Get outgoing flows
        outgoing_flows = elem.get('outgoing', [])
        
        if not outgoing_flows:
            return current_proc or "[]"
        
        # Check if any outgoing flow leads directly to target
        next_procs = []
        for flow_id in outgoing_flows:
            next_id = process_data['sequence_flows'][flow_id]['target']
            if next_id == target_id:
                # Reached target, stop here
                return current_proc or "[]"
            else:
                # Continue recursively
                next_proc = self._build_proc_until_target(next_id, target_id, process_data, visited.copy())
                if next_proc and next_proc != "[]":
                    next_procs.append(next_proc)
        
        # Combine current with next
        if not next_procs:
            return current_proc or "[]"
        
        if len(next_procs) == 1:
            next_proc = next_procs[0]
        else:
            # Multiple paths - shouldn't happen in well-formed process up to convergence
            next_proc = next_procs[0]
        
        if not current_proc:
            return next_proc
        
        if next_proc == "[]":
            return current_proc
        
        # Combine current and next
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
    
    def _find_exclusive_convergence(self, split_gateway_id, process_data):
        """
        Find the convergence point (merge gateway or common element) where all branches 
        from an exclusive gateway reconverge.
        
        Uses a level-by-level BFS approach to find the EARLIEST common element
        reachable from all branches at the same depth.
        
        Returns: (convergence_id, is_gateway) where:
            - convergence_id: the element ID where branches merge
            - is_gateway: True if it's an exclusive merge gateway, False if it's a regular element
        """
        split_elem = process_data['elements'].get(split_gateway_id)
        if not split_elem:
            return None, False
        
        outgoing_flows = split_elem.get('outgoing', [])
        if len(outgoing_flows) < 2:
            return None, False
        
        # Trace each branch independently level-by-level
        branch_levels = []  # List of [{elem_ids at depth 0}, {elem_ids at depth 1}, ...]
        
        for flow_id in outgoing_flows:
            target_id = process_data['sequence_flows'][flow_id]['target']
            levels = self._trace_by_levels(target_id, process_data, max_depth=20)
            branch_levels.append(levels)
        
        # Find the earliest depth where all branches reach a common element
        max_depth = max(len(levels) for levels in branch_levels)
        
        for depth in range(max_depth):
            # Get elements at this depth from each branch
            elements_at_depth = []
            for levels in branch_levels:
                if depth < len(levels):
                    elements_at_depth.append(levels[depth])
                else:
                    elements_at_depth.append(set())
            
            # Find intersection (common elements at this depth)
            if not elements_at_depth:
                continue
            
            common_at_depth = elements_at_depth[0].intersection(*elements_at_depth[1:])
            
            if common_at_depth:
                # Found convergence at this depth
                # Prefer merge gateways over regular elements
                for elem_id in common_at_depth:
                    elem = process_data['elements'].get(elem_id)
                    if elem and self._is_element_type(elem, EXCLUSIVE_GATEWAY):
                        # Check if it's a merge (multiple incoming)
                        if len(elem.get('incoming', [])) > 1:
                            return elem_id, True
                
                # No merge gateway found, return first common element
                conv_id = next(iter(common_at_depth))
                return conv_id, False
        
        return None, False
    
    def _trace_by_levels(self, start_id, process_data, max_depth=20):
        """
        Trace forward from start_id and return elements organized by depth level.
        Returns: [{elem_ids at depth 0}, {elem_ids at depth 1}, ...]
        """
        levels = []
        current_level = {start_id}
        visited = set()
        
        for depth in range(max_depth):
            if not current_level:
                break
            
            levels.append(current_level.copy())
            next_level = set()
            
            for elem_id in current_level:
                if elem_id in visited:
                    continue
                visited.add(elem_id)
                
                elem = process_data['elements'].get(elem_id)
                if elem:
                    for flow_id in elem.get('outgoing', []):
                        next_id = process_data['sequence_flows'].get(flow_id, {}).get('target')
                        if next_id and next_id not in visited:
                            next_level.add(next_id)
            
            current_level = next_level
        
        return levels
    
    def _generate_initial_state(self, fluents):
        code = INITIAL_SITUATION
        
        # Add initially clauses for FUNCTIONAL gateway fluents only
        # Relational fluents (boolean) don't need initialization
        # Loop fluents take precedence over general gateway fluents
        all_gateway_inits = {}
        all_gateway_inits.update(self.gateway_fluents)  # General gateways
        all_gateway_inits.update(self.loop_fluents)     # Loop fluents override
        
        # Filter to only functional fluents (non-boolean)
        functional_inits = {name: val for name, val in all_gateway_inits.items() 
                           if name in self.functional_fluents or name in self.loop_fluents}
        
        if functional_inits:
            code += "\n% Initial values for gateway choice fluents\n"
            for fluent_name, default_value in sorted(functional_inits.items()):
                code += f"initially({fluent_name}(_ID), {default_value}).\n"
            code += "\n"
        
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
        
        # Add: active in the current pool (where the exception is triggered)
        # This is crucial to ensure the exception can only be triggered from the originating pool
        if pool_obj:
            conditions.append(f"active(ID, {current_pool_name})")
        
        # Add: active in the variable pool (covers any pool where the exception can propagate)
        # This means the instance must be active in at least one pool
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
        """
        Check if a task is a decision task (precedes an exclusive gateway with explicit labels).
        A decision task is one where the task's output determines which branch is taken,
        so it needs a 3-parameter end action: task(end, ID, RESULT).
        """
        task = process_data['elements'].get(task_id)
        if not task or not task['outgoing']: 
            return False
        
        next_elem_id = process_data['sequence_flows'][task['outgoing'][0]]['target']
        next_elem = process_data['elements'].get(next_elem_id)
        
        if not (next_elem and 'exclusiveGateway' in next_elem['type']):
            return False
        
        # Check if the gateway has at least one explicitly labeled outgoing flow
        # (even if the labels are just "true"/"false", the task is still a decision task)
        outgoing_flows = next_elem.get('outgoing', [])
        for flow_id in outgoing_flows:
            flow_info = process_data['sequence_flows'].get(flow_id, {})
            flow_label = flow_info.get('name', '').strip()
            if flow_label:  # Has an explicit label
                return True
        
        return False

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
        
        # Check if next element is a diverging exclusive gateway (not a convergence)
        if not (next_elem and 'exclusiveGateway' in next_elem['type'] and len(next_elem.get('outgoing', [])) > 1):
            return False, []
        
        # Use existing gateway analysis
        is_functional, branch_values_dict = self._analyze_gateway_branches(next_elem['id'], process_data)
        
        if is_functional:
            # Get all unique values except 'default'
            labeled_values = [v for v in branch_values_dict.values() if v != 'default']
            
            # If there's only one labeled value and a default, infer the opposite value
            if len(labeled_values) == 1 and 'default' in branch_values_dict.values():
                labeled_val = labeled_values[0]
                # If the labeled value is 'true', default is 'false', and vice versa
                if labeled_val == 'true':
                    values = ['true', 'false']
                elif labeled_val == 'false':
                    values = ['false', 'true']
                else:
                    # For non-boolean values, we can't infer - just use the labeled value
                    values = labeled_values
            else:
                values = labeled_values
            
            return True, values
        else:
            # Boolean decision
            return False, ['true', 'false']

    def _generate_footer(self):
        return FOOTER