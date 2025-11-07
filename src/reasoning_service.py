import re
from reason import IndiGologReasoner, parse_action_list


class ReasoningService:
    REASONING_TASKS = {
        'projection': {
            'name': 'Projection',
            'description': 'Check what would be true after executing a sequence of actions',
            'parameters': [
                {'name': 'fluent_name', 'type': 'text', 'label': 'Fluent Name', 'placeholder': 'e.g., application(1)'},
                {'name': 'actions', 'type': 'text', 'label': 'Action Sequence (comma-separated)', 'placeholder': 'e.g., job_needed(1),acquire(1,applicant)'},
                {'name': 'expected_value', 'type': 'dropdown', 'label': 'Expected Value', 'choices': ['true', 'false']}
            ]
        },
        'legality': {
            'name': 'Legality Check',
            'description': 'Verify if a sequence of actions is executable (all preconditions satisfied)',
            'parameters': [
                {'name': 'actions', 'type': 'text', 'label': 'Action Sequence (comma-separated)', 'placeholder': 'e.g., job_needed(1),prepare_application(end,1)'}
            ]
        },
        'execute': {
            'name': 'Process Execution',
            'description': 'Execute the full BPMN process',
            'parameters': []
        },
        'conformance': {
            'name': 'Conformance Checking',
            'description': 'Verify if an execution history conforms to the process specification. Enter actions in chronological order (first to last).',
            'parameters': [
                {'name': 'history_actions', 'type': 'text', 'label': 'History Actions (comma-separated, chronological order)', 'placeholder': 'e.g., job_needed(1), prepare_application(start,1), prepare_application(end,1)'}
            ]
        },
        'verify': {
            'name': 'Property Verification',
            'description': 'Verify specific properties over the entire process execution. Enter conditions as comma-separated expressions.',
            'parameters': [
                {'name': 'property', 'type': 'text', 'label': 'Property Conditions (comma-separated)', 'placeholder': 'e.g., signed_contract(id), not(done(application_finalised(id)))'}
            ]
        }
    }
        
    @staticmethod
    def _extract_clean_result(full_output):
        lines = full_output.split('\n')
        result_parts = []
        
        # Check what type of task this is based on final status messages
        is_projection = any('Query evaluation:' in line for line in lines)
        is_legality = any('Action sequence is' in line and ('EXECUTABLE' in line or 'NOT EXECUTABLE' in line) for line in lines)
        
        # For projection queries, show the actual query result (TRUE/FALSE)
        if is_projection:
            for line in lines:
                if 'Query evaluation:' in line:
                    # Extract TRUE or FALSE
                    if 'TRUE' in line:
                        result_parts.append("Query result: TRUE")
                    else:
                        result_parts.append("Query result: FALSE")
                    result_parts.append('-' * 70)
                    result_parts.append('')
                    break
        # For legality checks, use the final status line (EXECUTABLE/NOT EXECUTABLE)
        elif is_legality:
            # First check for program failure
            for line in lines:
                if 'PROGRAM: Program fails' in line or 'Program fails:' in line:
                    result_parts.append("RESULT: Action sequence is NOT EXECUTABLE (illegal - program fails)")
                    result_parts.append('-' * 70)
                    result_parts.append('')
                    break
            
            # If no failure found, check for success
            if not result_parts:
                for line in lines:
                    stripped = line.strip()
                    if stripped.startswith('✓') or stripped.startswith('✗'):
                        if 'Action sequence is' in line and ('EXECUTABLE' in line or 'NOT EXECUTABLE' in line):
                            # Extract just the message without the emoji
                            if 'NOT EXECUTABLE' in line:
                                result_parts.append("RESULT: Action sequence is NOT EXECUTABLE (illegal)")
                            else:
                                result_parts.append("RESULT: Action sequence is EXECUTABLE (legal)")
                            result_parts.append('-' * 70)
                            result_parts.append('')
                            break
        else:
            # For other tasks, use the standard RESULT: line
            result_line = None
            for line in lines:
                if 'RESULT:' in line:
                    result_line = line.strip()
                    result_parts.append(result_line)
                    break
            
            if result_line:
                result_parts.append('-' * 70)
                result_parts.append('')
        
        extracted_history_start = -1
        extracted_history_end = -1
        
        # First pass: look for EXTRACTED EXECUTION HISTORY
        for i, line in enumerate(lines):
            if 'EXTRACTED EXECUTION HISTORY' in line:
                # The opening border should be before this title line
                # Then there's another border line after the title
                # Look for: i-1 is border, i is title, i+1 is border, then content, then closing border
                if i > 0 and '=' in lines[i-1] and len(lines[i-1].strip()) > 10:
                    extracted_history_start = i - 1
                    # Skip the separator line (i+1) and look for closing border
                    border_count = 0
                    for j in range(i + 1, min(i + 30, len(lines))):
                        if '=' in lines[j] and len(lines[j].strip()) > 10:
                            border_count += 1
                            if border_count == 2:  # First is separator, second is closing
                                extracted_history_end = j
                                break
                break
        
        # If we found the extracted history section, use it
        if extracted_history_start >= 0 and extracted_history_end > extracted_history_start:
            result_parts.append('=' * 70)
            result_parts.append('EXECUTION HISTORY')
            result_parts.append('=' * 70)
            # Content starts after title line (i+1) which is a separator border, so actual content is at i+2
            # extracted_history_start is line i-1 (opening border)
            # Title is at extracted_history_start + 1
            # Separator border is at extracted_history_start + 2
            # Content starts at extracted_history_start + 3
            for i in range(extracted_history_start + 3, extracted_history_end):
                line_content = lines[i].rstrip()
                result_parts.append(line_content)
            result_parts.append('=' * 70)
            result_parts.append('')
        
        # Extract conformance checking details
        for i, line in enumerate(lines):
            if 'History to check:' in line:
                result_parts.append(line.strip())
                result_parts.append('')
                break
        
        # Extract timing/inference information
        for line in lines:
            # Match patterns like "43,834 inferences, 0.006 CPU in 0.006 seconds"
            if 'inferences' in line.lower() and ('cpu' in line.lower() or 'seconds' in line.lower()):
                # Clean up the line
                cleaned = line.strip()
                # Remove leading/trailing dashes or other separators
                cleaned = cleaned.strip('-').strip()
                if cleaned:
                    result_parts.append(cleaned)
                    result_parts.append('')
                break
        
        # Extract final status indicator (but not for projection/legality as they're already handled)
        if not is_projection and not is_legality:
            for line in lines:
                stripped = line.strip()
                if stripped.startswith('✓') or stripped.startswith('✗'):
                    if any(keyword in line for keyword in ['Process execution', 'History is', 'COMPLETED', 'CONFORMANT']):
                        result_parts.append(stripped)
                        break
        
        # If we have collected parts, join them
        if result_parts:
            return '\n'.join(result_parts)
        
        # Fallback: look for simple indicators
        if 'Query evaluation: TRUE' in full_output:
            return "✅ Query succeeded"
        elif 'Query evaluation: FALSE' in full_output:
            return "❌ Query failed"
        
        if 'Action sequence is EXECUTABLE' in full_output:
            return "✅ Action sequence is EXECUTABLE (legal)"
        elif 'Action sequence is NOT EXECUTABLE' in full_output:
            return "❌ Action sequence is NOT EXECUTABLE (illegal)"
        
        if result_line:
            return result_line
        
        return "Task completed"
    
    def __init__(self, model_name, model_base_dir=None):
        self.model_name = model_name
        self.model_base_dir = model_base_dir
        self.reasoner = None
        self._initialize_reasoner()
    
    def _initialize_reasoner(self):
        try:
            self.reasoner = IndiGologReasoner(self.model_name, self.model_base_dir)
            return True, "Reasoner initialized successfully"
        except FileNotFoundError as e:
            return False, str(e)
        except Exception as e:
            return False, f"Error initializing reasoner: {str(e)}"
    
    def get_available_tasks(self):
        return self.REASONING_TASKS
    
    def execute_task(self, task_id, parameters):
        if not self.reasoner:
            success, msg = self._initialize_reasoner()
            if not success:
                return False, msg
        
        if task_id not in self.REASONING_TASKS:
            return False, f"Unknown task ID: {task_id}"
        
        try:
            if task_id == 'projection':
                success, full_output = self._execute_projection(parameters)
                # For projection, the task always succeeds - success=False just means query result was FALSE
                # The actual result (TRUE/FALSE) is shown in the extracted output
                task_success = True
            elif task_id == 'legality':
                success, full_output = self._execute_legality(parameters)
                # For legality, the task always succeeds - success=False just means sequence not executable
                # The actual result (EXECUTABLE/NOT EXECUTABLE) is shown in the extracted output
                task_success = True
            elif task_id == 'execute':
                success, full_output = self._execute_process()
                task_success = success
            elif task_id == 'conformance':
                success, full_output = self._execute_conformance(parameters)
                task_success = success
            elif task_id == 'verify':
                success, full_output = self._execute_verify(parameters)
                task_success = success
            else:
                return False, f"Task {task_id} not implemented"
            
            clean_result = self._extract_clean_result(full_output)
            #clean_result = full_output
            return task_success, clean_result
        
        except Exception as e:
            return False, f"Error executing task: {str(e)}"
    
    def _execute_projection(self, params):
        fluent_name = params.get('fluent_name', '').strip()
        actions_str = params.get('actions', '').strip()
        expected_value = params.get('expected_value', 'true').strip()
        
        if not fluent_name:
            return False, "Fluent name is required"
        if not actions_str:
            return False, "Action sequence is required"
        
        actions = parse_action_list(actions_str)
        return self.reasoner.projection(fluent_name, actions, expected_value)
    
    def _execute_legality(self, params):
        actions_str = params.get('actions', '').strip()
        proc_name = params.get('proc_name', 'legality_check').strip()
        
        if not actions_str:
            return False, "Action sequence is required"
        
        actions = parse_action_list(actions_str)
        return self.reasoner.legality(proc_name, actions)
    
    def _execute_process(self):
        controller_number = 1
        return self.reasoner.execute_process(controller_number)
    
    def _execute_conformance(self, params):
        history_str = params.get('history_actions', '').strip()
        if not history_str:
            return False, "History actions are required"
        
        history_actions = parse_action_list(history_str)
        reversed_history = list(reversed(history_actions))
        return self.reasoner.conformance_checking(reversed_history)
    
    def _execute_verify(self, params):
        property_str = params.get('property', '').strip()
        if not property_str:
            return False, "Property conditions are required"
        
        proc_name = 'property_verification'
        property_str = re.sub(r'\bnot\s*\(', 'neg(', property_str)
        conditions = []
        current_condition = ""
        paren_depth = 0
        
        for char in property_str:
            if char == '(':
                paren_depth += 1
                current_condition += char
            elif char == ')':
                paren_depth -= 1
                current_condition += char
            elif char == ',' and paren_depth == 0:
                if current_condition.strip():
                    conditions.append(current_condition.strip())
                current_condition = ""
            else:
                current_condition += char
        
        if current_condition.strip(): # Last condition
            conditions.append(current_condition.strip())
        
        if len(conditions) == 0:   # Build the Prolog property with nested 'and'
            return False, "At least one condition is required"
        elif len(conditions) == 1:
            property_expr = conditions[0]
        else:
            property_expr = conditions[-1]
            for cond in reversed(conditions[:-1]):
                property_expr = f"and({cond}, {property_expr})"
        
        return self.reasoner.verify_property(property_expr, proc_name)
