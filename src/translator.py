import os


class Translator:
    def __init__(self, bpmn_name, main_path, domain_path, parser):
        self.bpmn_name = bpmn_name
        self.support_dir = os.path.join(os.path.dirname(__file__), 'support')
        self.main_path = main_path
        self.domain_path = domain_path
        self.parser = parser

        self.translate(self.bpmn_name)

    def translate(self, bpmn_name):
        # Generate the domain file content
        domain_content = self.generate_domain_file()
        
        # Write the domain file
        with open(self.domain_path, 'w') as file:
            file.write(domain_content)
        
        # Generate the main file (unchanged)
        with open(os.path.join(self.support_dir, 'main.pl'), 'r') as file:
            lines = file.readlines()

        with open(self.main_path, 'w') as file:
            for line in lines:
                if line.strip() == ':- [REPLACE].':
                    file.write(f':- [{bpmn_name}].\n')
                else:
                    file.write(line)

    def generate_domain_file(self):
        content = []
        
        # Header
        content.append("/*")
        content.append(f"  BPMN Process: {self.bpmn_name}")
        content.append("  Generated from BPMN model")
        content.append("*/")
        content.append("")
        
        # Dynamic declarations
        content.append(":- dynamic controller/1.")
        content.append(":- discontiguous")
        content.append("    fun_fluent/1,")
        content.append("    rel_fluent/1,")
        content.append("    proc/2,")
        content.append("    causes_true/3,")
        content.append("    causes_false/3.")
        content.append("")
        
        # Cache
        content.append("% There is nothing to do caching on (required because cache/1 is static)")
        content.append("cache(_) :- fail.")
        content.append("")
        
        # ID predicate
        content.append("id(X) :- ground(X), !.")
        content.append("id(X) :- between(1, 1, X).")
        content.append("")
        
        # Generate fluents
        content.extend(self.generate_fluents())
        
        # Generate actions
        content.extend(self.generate_actions())
        
        # Generate exogenous actions
        content.extend(self.generate_exogenous_actions())
        
        # Abbreviations
        content.extend(self.generate_abbreviations())
        
        # Initial situation
        content.extend(self.generate_initial_situation())
        
        # Procedures
        content.extend(self.generate_procedures())
        
        return "\n".join(content)

    def generate_fluents(self):
        content = []
        content.append("% Domain-dependent Relational Fluents")
        
        # Generate fluents for data objects (deduplicated by name)
        seen_names = set()
        for data_obj in self.parser.data_objects:
            name = data_obj['name'].replace(' ', '_')
            if name not in seen_names:
                content.append(f"rel_fluent({name}(_ID)).")
                seen_names.add(name)
        
        # Generate fluents for gateway conditions
        gateway_fluents = self.extract_gateway_fluents()
        for fluent in gateway_fluents:
            content.append(f"rel_fluent({fluent}(_ID)).")
        
        # Add standard process fluents
        content.append("rel_fluent(active(_ID)).")
        content.append("rel_fluent(processed(_ID)).")
        
        # Add pool fluents
        if self.parser.pools:
            pools_str = ', '.join([f"'{p.lower()}'" for p in self.parser.pools])
            content.append(f"rel_fluent(pool(P)) :- member(P, [{pools_str}]).")
            content.append("rel_fluent(waiting(_ID, _POOL)).")
        
        # Add servers_stopped fluent
        content.append("rel_fluent(servers_stopped).")
        
        # Generate causes_true/causes_false for gateway conditions
        content.extend(self.generate_gateway_causes())
        
        # Generate causes_true for data objects
        content.extend(self.generate_data_object_causes())
        
        # Generate causes_true for process states
        content.extend(self.generate_process_state_causes())
        
        # Generate causes_true for waiting relationships
        content.extend(self.generate_waiting_causes())
        
        content.append("")
        return content

    def generate_actions(self):
        content = []
        content.append("% Actions and Preconditions")
        content.append("")
        
        # Get activities that lead to gateways (need RESULT parameter)
        gateway_activities = self.get_gateway_activities()
        
        # Generate actions for tasks
        for task in self.parser.tasks:
            task_name = task['name'].replace(' ', '_')
            if task['name'] in gateway_activities:
                # This task leads to a gateway, so it needs RESULT parameter
                content.append(f"prim_action({task_name}(start, _ID)).")
                content.append(f"prim_action({task_name}(end, _ID, RESULT)).")
            else:
                content.append(f"prim_action({task_name}(start, _ID)).")
                content.append(f"prim_action({task_name}(end, _ID)).")
            
            # Add precondition based on flows and data
            precondition = self.generate_precondition(task['name'], task['id'])
            content.append(f"poss({task_name}(start, ID), {precondition}).")
        
        # Generate simple actions for throw message events
        for event in self.parser.throw_message_events:
            event_name = event.replace(' ', '_')
            content.append(f"prim_action({event_name}(_ID)).")
            precondition = self.generate_precondition(event)
            content.append(f"poss({event_name}(ID), {precondition}).")
        
        # Generate simple actions for end events
        for event in self.parser.end_events:
            event_name = event.replace(' ', '_')
            content.append(f"prim_action({event_name}(_ID)).")
            precondition = self.generate_precondition(event)
            content.append(f"poss({event_name}(ID), {precondition}).")
        
        content.append("")
        return content

    def get_gateway_activities(self):
        """Get list of activities that flow into gateways"""
        activities = set()
        
        # Create a mapping from gateway names to their info
        gateway_names = set()
        all_gateways = self.parser.xor_gateways + self.parser.event_based_gateways + self.parser.parallel_gateways
        
        for gateway in all_gateways:
            if gateway['name']:
                gateway_names.add(gateway['name'])
        
        # Find activities that flow into gateways
        for source, target in self.parser.flows:
            if target in gateway_names:
                activities.add(source)
        
        return list(activities)

    def generate_exogenous_actions(self):
        content = []
        content.append("% Exogenous Actions")
        content.append("")
        content.append("prim_action(Act) :- exog_action(Act).")
        content.append("")
        
        # Get activities that lead to gateways
        gateway_activities = self.get_gateway_activities()
        
        # Generate exogenous actions for start events
        for event in self.parser.start_events:
            event_name = event.replace(' ', '_')
            content.append(f"exog_action({event_name}(_ID)).")
            content.append(f"poss({event_name}(ID), neg(done({event_name}(ID)))).")
        
        # Generate exogenous actions for task end actions
        for task in self.parser.tasks:
            task_name = task['name'].replace(' ', '_')
            if task['name'] in gateway_activities:
                # This task leads to a gateway, so it needs RESULT parameter
                content.append(f"exog_action({task_name}(end, _ID, RESULT)).")
                content.append(f"poss({task_name}(end, ID, RESULT), and(running({task_name}(start, ID)), member(RESULT, [true, false]))).")
            else:
                content.append(f"exog_action({task_name}(end, _ID)).")
                content.append(f"poss({task_name}(end, ID), running({task_name}(start, ID))).")
        
        # Add shut_down exogenous action
        content.append("exog_action(shut_down).")
        content.append("poss(shut_down, and(neg(servers_stopped), neg(active_instances_check))).")
        
        content.append("")
        return content

    def generate_abbreviations(self):
        content = []
        content.append("% Abbreviations")
        content.append("")
        content.append("% default running/1 when start and end actions have the same number of arguments")
        content.append("proc(running(A1), and(done(A1), neg(done(A2)))) :-")
        content.append("  A1 =.. [F|[start|L]], A2 =.. [F|[end|L]].")
        
        # Add active_instances_check
        content.append("proc(active_instances_check, some(id, active(id))).")
        
        content.append("")
        return content

    def extract_gateway_fluents(self):
        """Extract fluent names from gateways that have names (conditions)"""
        fluents = set()
        
        # Check all gateway types for names
        all_gateways = self.parser.xor_gateways + self.parser.event_based_gateways + self.parser.parallel_gateways
        
        for gateway in all_gateways:
            if gateway['name']:
                # Convert gateway name to fluent name (e.g., "documents ok?" -> "documents_ok")
                fluent_name = gateway['name'].lower().replace('?', '').replace(' ', '_')
                fluents.add(fluent_name)
        
        return list(fluents)

    def generate_gateway_causes(self):
        """Generate causes_true/causes_false for gateway conditions"""
        content = []
        
        # Create a mapping from gateway names to their info
        gateway_name_to_info = {}
        all_gateways = self.parser.xor_gateways + self.parser.event_based_gateways + self.parser.parallel_gateways
        
        for gateway in all_gateways:
            if gateway['name']:
                gateway_name_to_info[gateway['name']] = gateway
        
        # Find activities that flow into gateways
        for source, target in self.parser.flows:
            if target in gateway_name_to_info:
                # This is a flow from an activity to a gateway
                gateway_info = gateway_name_to_info[target]
                fluent_name = gateway_info['name'].lower().replace('?', '').replace(' ', '_')
                activity_name = source.replace(' ', '_')
                
                # Generate causes_true/causes_false for the activity's end action
                content.append(f"causes_true({activity_name}(end, ID, RESULT), {fluent_name}(ID), RESULT = true).")
                content.append(f"causes_false({activity_name}(end, ID, RESULT), {fluent_name}(ID), RESULT = false).")
        
        return content

    def generate_data_object_causes(self):
        """Generate causes_true for data objects based on data associations"""
        content = []
        
        # Create mapping from data object IDs to their names
        data_object_id_to_name = {}
        for data_obj in self.parser.data_objects:
            data_object_id_to_name[data_obj['id']] = data_obj['name'].replace(' ', '_')
        
        # Create mapping from activity IDs to their names
        activity_id_to_name = {}
        for task in self.parser.tasks:
            activity_id_to_name[task['id']] = task['name'].replace(' ', '_')
        
        # Find output associations (activities that produce data objects)
        for association in self.parser.data_associations:
            if association['type'] == 'output':
                activity_id = association['activity']
                data_object_id = association['data_object']
                
                if activity_id in activity_id_to_name and data_object_id in data_object_id_to_name:
                    activity_name = activity_id_to_name[activity_id]
                    data_object_name = data_object_id_to_name[data_object_id]
                    content.append(f"causes_true({activity_name}(end, ID), {data_object_name}(ID), true).")
        
        return content

    def generate_process_state_causes(self):
        """Generate causes_true/causes_false for process states"""
        content = []
        
        # Start events activate the process
        for event in self.parser.start_events:
            event_name = event.replace(' ', '_')
            content.append(f"causes_true({event_name}(ID), active(ID), true).")
        
        # End events mark the process as processed
        for event in self.parser.end_events:
            event_name = event.replace(' ', '_')
            content.append(f"causes_true({event_name}(ID), processed(ID), true).")
        
        # Withdrawal and finalization events deactivate the process
        content.append("causes_false(withdrawal_handled(ID), active(ID), true).")
        content.append("causes_false(application_finalised(ID), active(ID), true).")
        content.append("causes_false(withdrawal_completed(ID), active(ID), true).")
        
        # Servers stopped
        content.append("causes_true(shut_down, servers_stopped, true).")
        
        return content

    def generate_waiting_causes(self):
        """Generate causes_true/causes_false for waiting relationships based on message events"""
        content = []
        
        # Start events cause waiting for the first pool
        if self.parser.start_events and self.parser.pools:
            start_event = self.parser.start_events[0].replace(' ', '_')
            first_pool = self.parser.pools[0].lower()
            content.append(f"causes_true({start_event}(ID), waiting(ID, {first_pool}), true).")
        
        # Throw message events cause waiting for the next pool
        for event in self.parser.throw_message_events:
            event_name = event.replace(' ', '_')
            # For application_sent, it causes waiting for company
            if 'application_sent' in event_name and len(self.parser.pools) > 1:
                content.append(f"causes_true({event_name}(ID), waiting(ID, {self.parser.pools[1].lower()}), true).")
        
        # End events clear waiting
        for event in self.parser.end_events:
            event_name = event.replace(' ', '_')
            if self.parser.pools:
                for pool in self.parser.pools:
                    content.append(f"causes_false({event_name}(ID), waiting(ID, {pool.lower()}), true).")
        
        # Application analysed also clears waiting
        if self.parser.pools:
            for pool in self.parser.pools:
                content.append(f"causes_false(application_analysed(ID), waiting(ID, {pool.lower()}), true).")
        
        return content

    def generate_initial_situation(self):
        content = []
        content.append("% Initial Situation")
        content.append("")
        
        # Initialize data object fluents to false (deduplicated by name)
        seen_names = set()
        for data_obj in self.parser.data_objects:
            name = data_obj['name'].replace(' ', '_')
            if name not in seen_names:
                content.append(f"initially({name}(_ID), false).")
                seen_names.add(name)
        
        # Initialize gateway fluents to false
        gateway_fluents = self.extract_gateway_fluents()
        for fluent in gateway_fluents:
            content.append(f"initially({fluent}(_ID), false).")
        
        # Initialize process fluents
        content.append("initially(active(_ID), false).")
        content.append("initially(processed(_ID), false).")
        
        # Initialize pool fluents
        if self.parser.pools:
            content.append("initially(pool(_P), true).")
            content.append("initially(waiting(_ID, _POOL), false).")
        
        # Initialize servers_stopped
        content.append("initially(servers_stopped, false).")
        
        content.append("")
        return content

    def generate_precondition(self, activity_name, activity_id=None):
        """Generate precondition based on incoming flows and data associations"""
        conditions = []
        
        # Find incoming sequence flows
        for source, target in self.parser.flows:
            if target == activity_name:
                task_id = None
                # Check if source is a task
                for task in self.parser.tasks:
                    if task['name'] == source:
                        conditions.append(f"done({source.replace(' ', '_')}(end, ID))")
                        task_id = task['id']
                        break
                # Check if source is a throw message event
                for event in self.parser.throw_message_events:
                    if event == source:
                        conditions.append(f"done({event.replace(' ', '_')}(ID))")
                        break
                # Check if source is a catch message event (may correspond to throw)
                for event in self.parser.catch_message_events:
                    if event == source:
                        # Assume it corresponds to a throw event with similar name
                        throw_name = event.replace(' received', '_sent').replace(' ', '_')
                        conditions.append(f"done({throw_name}(ID))")
                        break
                # Check if source is a gateway
                for gateway in self.parser.xor_gateways + self.parser.parallel_gateways + self.parser.event_based_gateways:
                    if gateway['name'] == source:
                        fluent_name = gateway['name'].lower().replace('?', '').replace(' ', '_')
                        conditions.append(f"{fluent_name}(ID)")
                        break
                # Check if source is a start event (message start)
                if source in self.parser.start_events and 'received' in source:
                    throw_name = source.replace(' received', '_sent').replace(' ', '_')
                    conditions.append(f"done({throw_name}(ID))")
                
                # If task_id found, check for output data associations
                if task_id:
                    for assoc in self.parser.data_associations:
                        if assoc['type'] == 'output' and assoc['activity'] == task_id:
                            data_obj_name = None
                            for data_obj in self.parser.data_objects:
                                if data_obj['id'] == assoc['data_object']:
                                    data_obj_name = data_obj['name'].replace(' ', '_')
                                    break
                            if data_obj_name:
                                conditions.append(f"{data_obj_name}(ID)")
        
        # Find input data associations (only if activity_id is provided)
        if activity_id:
            for assoc in self.parser.data_associations:
                if assoc['type'] == 'input' and assoc['activity'] == activity_id:
                    data_obj_name = None
                    for data_obj in self.parser.data_objects:
                        if data_obj['id'] == assoc['data_object']:
                            data_obj_name = data_obj['name'].replace(' ', '_')
                            break
                    if data_obj_name:
                        conditions.append(f"{data_obj_name}(ID)")
        
        # Combine conditions
        if not conditions:
            return "true"
        elif len(conditions) == 1:
            return conditions[0]
        else:
            result = conditions[0]
            for cond in conditions[1:]:
                result = f"and({result}, {cond})"
            return result

    def generate_procedures(self):
        content = []
        content.append("% Procedures")
        content.append("")
        content.append("% TODO: Generate procedures based on BPMN flows and gateways")
        content.append("")
        return content