"""
Reasoning Service Module

This module provides a clean API for all reasoning tasks over IndiGolog BPMN translations,
wrapping the IndiGologReasoner functionality and providing a simple interface for the UI.
"""

from reason import IndiGologReasoner, parse_action_list


class ReasoningService:
    """Service class for reasoning tasks over IndiGolog translations."""
    
    @staticmethod
    def _extract_clean_result(full_output):
        """
        Extract a clean result message from the Prolog output.
        
        Args:
            full_output: The full output from Prolog execution
        
        Returns:
            str: A clean, user-friendly result message with relevant details
        """
        lines = full_output.split('\n')
        result_parts = []
        
        # Check for specific errors
        if 'eaddrinuse' in full_output or 'Address already in use' in full_output:
            return ("❌ ERROR: Port already in use\n\n"
                   "The Environment Manager port (8000) is still in use from a previous execution.\n"
                   "This happens when a previous reasoning task didn't shut down properly.\n\n"
                   "The system will now attempt cleanup. Please try again in a moment.")
        
        # Extract RESULT line
        result_line = None
        for line in lines:
            if 'RESULT:' in line:
                result_line = line.strip()
                result_parts.append(result_line)
                break
        
        # Add separator after result
        if result_line:
            result_parts.append('-' * 70)
            result_parts.append('')
        
        # Extract execution history section - look for both formats
        # Format 1: EXECUTION HISTORY (Action Sequence): with ========================================
        # Format 2: EXTRACTED EXECUTION HISTORY with ======================================================================
        in_history_section = False
        history_lines = []
        border_count = 0
        
        for i, line in enumerate(lines):
            # Detect start of history section
            if not in_history_section:
                if '========' in line:
                    # Look ahead to see if next line has history-related keywords
                    if i + 1 < len(lines):
                        next_line = lines[i + 1]
                        if 'EXECUTION HISTORY' in next_line or 'EXTRACTED EXECUTION HISTORY' in next_line:
                            in_history_section = True
                            history_lines.append('=' * 70)
                            border_count = 1
                            continue
            else:
                # We're in the history section
                if '========' in line:
                    # This is the closing border
                    history_lines.append('=' * 70)
                    in_history_section = False
                    break
                else:
                    # Include all content lines (including empty ones for spacing)
                    history_lines.append(line.rstrip())
        
        if history_lines and len(history_lines) > 2:  # Only add if we have actual content
            result_parts.extend(history_lines)
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
        
        # Extract final status indicator
        for line in lines:
            stripped = line.strip()
            if stripped.startswith('✓') or stripped.startswith('✗'):
                if any(keyword in line for keyword in ['Process execution', 'History is', 'Action sequence is', 'COMPLETED', 'CONFORMANT']):
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
    
    # Define available reasoning tasks with their required parameters
    REASONING_TASKS = {
        'projection': {
            'name': 'Projection',
            'description': 'Check what would be true after executing a sequence of actions',
            'parameters': [
                {'name': 'fluent_name', 'type': 'text', 'label': 'Fluent Name', 'placeholder': 'e.g., door_open'},
                {'name': 'actions', 'type': 'text', 'label': 'Action Sequence (comma-separated)', 'placeholder': 'e.g., open,close,open'},
                {'name': 'expected_value', 'type': 'dropdown', 'label': 'Expected Value', 'choices': ['true', 'false']}
            ]
        },
        'legality': {
            'name': 'Legality Check',
            'description': 'Verify if a sequence of actions is executable (all preconditions satisfied)',
            'parameters': [
                {'name': 'actions', 'type': 'text', 'label': 'Action Sequence (comma-separated)', 'placeholder': 'e.g., job_needed(1),prepare_application(end,1)'},
                {'name': 'proc_name', 'type': 'text', 'label': 'Procedure Name (optional)', 'placeholder': 'legality_check', 'default': 'legality_check'}
            ]
        },
        'execute': {
            'name': 'Process Execution',
            'description': 'Execute the full BPMN process with a specific controller',
            'parameters': [
                {'name': 'controller_number', 'type': 'number', 'label': 'Controller Number', 'placeholder': '1', 'default': 1}
            ]
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
            'description': 'Execute a reasoning task procedure to verify a specific property',
            'parameters': [
                {'name': 'proc_name', 'type': 'text', 'label': 'Procedure Name', 'placeholder': 'reasoning_task', 'default': 'reasoning_task'}
            ]
        }
    }
    
    def __init__(self, model_name):
        """
        Initialize the reasoning service for a specific model.
        
        Args:
            model_name: The name of the BPMN model
        """
        self.model_name = model_name
        self.reasoner = None
        self._initialize_reasoner()
    
    def _initialize_reasoner(self):
        """Initialize the IndiGolog reasoner."""
        try:
            self.reasoner = IndiGologReasoner(self.model_name)
            return True, "Reasoner initialized successfully"
        except FileNotFoundError as e:
            return False, str(e)
        except Exception as e:
            return False, f"Error initializing reasoner: {str(e)}"
    
    def get_available_tasks(self):
        """
        Get the list of available reasoning tasks.
        
        Returns:
            dict: Dictionary of task IDs to task information
        """
        return self.REASONING_TASKS
    
    def execute_task(self, task_id, parameters):
        """
        Execute a reasoning task with the given parameters.
        
        Args:
            task_id: The ID of the task to execute (e.g., 'projection', 'legality')
            parameters: Dictionary of parameter names to values
        
        Returns:
            tuple: (success: bool, clean_output: str)
        """
        if not self.reasoner:
            success, msg = self._initialize_reasoner()
            if not success:
                return False, msg
        
        if task_id not in self.REASONING_TASKS:
            return False, f"Unknown task ID: {task_id}"
        
        try:
            if task_id == 'projection':
                success, full_output = self._execute_projection(parameters)
            elif task_id == 'legality':
                success, full_output = self._execute_legality(parameters)
            elif task_id == 'execute':
                success, full_output = self._execute_process(parameters)
            elif task_id == 'conformance':
                success, full_output = self._execute_conformance(parameters)
            elif task_id == 'verify':
                success, full_output = self._execute_verify(parameters)
            else:
                return False, f"Task {task_id} not implemented"
            
            # Extract clean result
            #clean_result = self._extract_clean_result(full_output)
            clean_result = full_output
            return success, clean_result
        
        except Exception as e:
            return False, f"Error executing task: {str(e)}"
    
    def _execute_projection(self, params):
        """Execute projection task."""
        fluent_name = params.get('fluent_name', '').strip()
        actions_str = params.get('actions', '').strip()
        expected_value = params.get('expected_value', 'true').strip()
        
        if not fluent_name:
            return False, "Fluent name is required"
        if not actions_str:
            return False, "Action sequence is required"
        
        # Parse action list
        actions = parse_action_list(actions_str)
        
        # Execute projection
        return self.reasoner.projection(fluent_name, actions, expected_value)
    
    def _execute_legality(self, params):
        """Execute legality check task."""
        actions_str = params.get('actions', '').strip()
        proc_name = params.get('proc_name', 'legality_check').strip()
        
        if not actions_str:
            return False, "Action sequence is required"
        
        # Parse action list
        actions = parse_action_list(actions_str)
        
        # Execute legality check
        return self.reasoner.legality(proc_name, actions)
    
    def _execute_process(self, params):
        """Execute process execution task."""
        controller_number = params.get('controller_number', 1)
        
        # Ensure controller_number is an integer
        try:
            controller_number = int(controller_number)
        except (ValueError, TypeError):
            return False, "Controller number must be an integer"
        
        # Execute process
        return self.reasoner.execute_process(controller_number)
    
    def _execute_conformance(self, params):
        """Execute conformance checking task.
        
        Note: User enters actions in chronological order (first to last),
        but the underlying Prolog script expects them in reverse order (last to first).
        This method handles the reversal automatically.
        """
        history_str = params.get('history_actions', '').strip()
        
        if not history_str:
            return False, "History actions are required"
        
        # Parse action list (user provides chronological order: first -> last)
        history_actions = parse_action_list(history_str)
        
        # Reverse the list for Prolog (expects: last -> first)
        # The Prolog conformance check query uses the list in reverse chronological order
        reversed_history = list(reversed(history_actions))
        
        # Execute conformance check with reversed history
        return self.reasoner.conformance_checking(reversed_history)
    
    def _execute_verify(self, params):
        """Execute property verification task."""
        proc_name = params.get('proc_name', 'reasoning_task').strip()
        
        if not proc_name:
            return False, "Procedure name is required"
        
        # Execute property verification
        return self.reasoner.verify_property(proc_name)
