#!/usr/bin/env python3
"""
Reasoning Script for IndiGolog BPMN Translations

This script provides an interface to perform reasoning tasks over IndiGolog translations
of BPMN models. It supports various reasoning tasks such as projection, legality checks, 
process execution, property verification, etc.

Usage:
    python reason.py <model_name> projection --fluent <fluent_name> --actions <action_list> --expected <true|false>
    python reason.py <model_name> legality --actions <action_list> [--proc-name <name>]
    python reason.py <model_name> execute [--controller <number>]
    python reason.py <model_name> verify --property <property_expression> [--proc-name <name>]

Examples:
    python reason.py job_application projection --fluent door_open --actions open,close --expected true
    python reason.py job_application legality --actions "job_needed(1),prepare_application(end,1)"
    python reason.py job_application execute --controller 1
    python reason.py job_application verify --property "signed_contract(id), neg(done(application_finalised(id)))"
"""

import argparse
import os
import sys
import subprocess
import tempfile
import signal
import re


def parse_action_list(action_string):
    """
    Parse a comma-separated action list, respecting parentheses.
    
    This handles cases where actions have arguments with commas, e.g.:
    "action1(a,b),action2,action3(x,y,z)"
    
    Args:
        action_string: String containing comma-separated actions
    
    Returns:
        List of action strings
    """
    actions = []
    current_action = ""
    paren_depth = 0
    
    for char in action_string:
        if char == '(':
            paren_depth += 1
            current_action += char
        elif char == ')':
            paren_depth -= 1
            current_action += char
        elif char == ',' and paren_depth == 0:
            # This comma is a separator between actions
            if current_action.strip():
                actions.append(current_action.strip())
            current_action = ""
        else:
            current_action += char
    
    # Don't forget the last action
    if current_action.strip():
        actions.append(current_action.strip())
    
    return actions


class IndiGologReasoner:
    """
    A class to handle reasoning tasks over IndiGolog BPMN translations.
    """
    
    def __init__(self, model_name):
        """
        Initialize the reasoner with a specific model.
        
        Args:
            model_name: The name of the BPMN model (without extension)
        """
        self.model_name = model_name
        self.script_dir = os.path.dirname(os.path.realpath(__file__))
        self.project_root = os.path.abspath(os.path.join(self.script_dir, '..'))
        self.indigolog_dir = os.path.join(self.project_root, 'indigolog')
        self.config_pl = os.path.join(self.indigolog_dir, 'config.pl')
        self.model_dir = os.path.join(self.project_root, 'pl_models', model_name)
        self.main_pl = os.path.join(self.model_dir, 'main.pl')
        
        # Validate paths
        self._validate_setup()
    
    def _validate_setup(self):
        """Validate that all required files and directories exist."""
        if not os.path.exists(self.indigolog_dir):
            raise FileNotFoundError(
                f"IndiGolog directory not found: {self.indigolog_dir}"
            )
        
        if not os.path.exists(self.config_pl):
            raise FileNotFoundError(
                f"IndiGolog config.pl not found: {self.config_pl}"
            )
        
        if not os.path.exists(self.model_dir):
            raise FileNotFoundError(
                f"Model directory not found: {self.model_dir}\n"
                f"Please run the translation first: python main.py {self.model_name}"
            )
        
        if not os.path.exists(self.main_pl):
            raise FileNotFoundError(
                f"Main Prolog file not found: {self.main_pl}\n"
                f"Please run the translation first: python main.py {self.model_name}"
            )
    
    def projection(self, fluent_name, actions, expected_value):
        """
        Perform a projection task: given a sequence of actions, determine what would be 
        true/false in the situation that results from performing that sequence.
        
        Args:
            fluent_name: The name of the fluent to evaluate
            actions: List of actions in normal order (will be reversed for Prolog)
            expected_value: Expected truth value (true or false)
        
        Returns:
            tuple: (success: bool, output: str)
        """
        # Reverse the action sequence for Prolog (as per IndiGolog convention)
        reversed_actions = list(reversed(actions))
        
        # Format the action list for Prolog - each action should be an atom/term
        # Join actions with ', ' to create proper list syntax
        action_list_str = '[' + ', '.join(reversed_actions) + ']'
        
        # Create the Prolog query - properly escape if needed
        query = f"eval({fluent_name}, {action_list_str}, {expected_value})"
        
        print(f"\n{'='*70}")
        print(f"PROJECTION TASK")
        print(f"{'='*70}")
        print(f"Model:           {self.model_name}")
        print(f"Fluent:          {fluent_name}")
        print(f"Action sequence: {' -> '.join(actions)}")
        print(f"Expected value:  {expected_value}")
        print(f"Prolog query:    ?- {query}.")
        print(f"{'='*70}\n")
        
        return self._run_prolog_query(query)
    
    def _kill_processes_on_port(self, port=8000):
        """
        Kill any processes using the specified port.
        
        Args:
            port: Port number to check (default: 8000 for Environment Manager)
        """
        try:
            # Use lsof to find processes using the port
            result = subprocess.run(
                ['lsof', '-ti', f':{port}'],
                capture_output=True,
                text=True,
                timeout=5
            )
            
            if result.stdout.strip():
                pids = result.stdout.strip().split('\n')
                for pid in pids:
                    if pid:
                        try:
                            print(f"Killing process {pid} using port {port}...")
                            os.kill(int(pid), signal.SIGKILL)
                        except Exception as e:
                            print(f"Warning: Could not kill process {pid}: {e}")
        except FileNotFoundError:
            # lsof not available, try fuser
            try:
                subprocess.run(
                    ['fuser', '-k', f'{port}/tcp'],
                    capture_output=True,
                    timeout=5
                )
            except:
                pass
        except Exception as e:
            print(f"Warning: Error checking/killing processes on port {port}: {e}")
    
    def _run_swipl_with_cleanup(self, cmd, temp_file_path, timeout=300):
        """
        Run a SWI-Prolog command with proper process cleanup.
        
        Args:
            cmd: Command list to execute
            temp_file_path: Path to temporary file (for cleanup)
            timeout: Timeout in seconds (default: 300/5 minutes)
        
        Returns:
            tuple: (success: bool, output: str, returncode: int)
        """
        process = None
        try:
            # Clean up any lingering processes on port 8000 before starting
            self._kill_processes_on_port(8000)
            
            # Run the command with Popen for better process management
            process = subprocess.Popen(
                cmd,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True,
                cwd=self.indigolog_dir,
                preexec_fn=os.setsid if hasattr(os, 'setsid') else None  # Create new process group on Unix
            )
            
            # Wait for completion with timeout
            try:
                stdout, stderr = process.communicate(timeout=timeout)
                full_output = stdout + stderr
                returncode = process.returncode
                return True, full_output, returncode
            except subprocess.TimeoutExpired:
                print(f"WARNING: Process timed out after {timeout} seconds. Terminating...")
                # Kill the entire process group
                if hasattr(os, 'killpg'):
                    try:
                        os.killpg(os.getpgid(process.pid), signal.SIGKILL)
                    except:
                        pass
                else:
                    process.terminate()
                try:
                    process.wait(timeout=5)
                except:
                    process.kill()
                return False, f"ERROR: Process timed out after {timeout} seconds", 1
                
        except Exception as e:
            return False, f"ERROR: {str(e)}", 1
            
        finally:
            # Ensure the process is terminated
            if process is not None:
                try:
                    if process.poll() is None:
                        # Process still running
                        if hasattr(os, 'killpg'):
                            try:
                                os.killpg(os.getpgid(process.pid), signal.SIGKILL)
                            except:
                                pass
                        else:
                            process.terminate()
                        try:
                            process.wait(timeout=3)
                        except:
                            try:
                                process.kill()
                            except:
                                pass
                except Exception as e:
                    print(f"Warning: Error during process cleanup: {e}")
            
            # Kill any remaining processes on port 8000
            self._kill_processes_on_port(8000)
            
            # Give the system a moment to release the port
            import time
            time.sleep(0.5)
            
            # Clean up temporary file
            try:
                os.unlink(temp_file_path)
            except:
                pass
    
    def _run_prolog_query(self, query):
        """
        Run a Prolog query using swipl.
        
        Args:
            query: The Prolog query to execute
        
        Returns:
            tuple: (success: bool, output: str)
        """
        # Create a temporary file with the query
        with tempfile.NamedTemporaryFile(mode='w', suffix='.pl', delete=False) as temp_file:
            temp_file_path = temp_file.name
            # Write a goal that will execute the query and halt
            # Use proper Prolog syntax with parentheses on same line
            temp_file.write(":- initialization(run_query).\n")
            temp_file.write("run_query :- (\n")
            temp_file.write(f"    {query} ->\n")
            temp_file.write("        (writeln('RESULT: SUCCESS - Query succeeded.'), halt(0))\n")
            temp_file.write("    ;\n")
            temp_file.write("        (writeln('RESULT: FAILURE - Query failed.'), halt(1))\n")
            temp_file.write("    ).\n")
        
        try:
            # Build the swipl command
            # We load config.pl, then main.pl, then our query file
            cmd = [
                'swipl',
                '-g', 'true',  # Don't start with a goal yet
                '-t', 'halt',   # Exit after loading
                self.config_pl,
                self.main_pl,
                temp_file_path
            ]
            
            print("Executing Prolog query...")
            print(f"Command: {' '.join(cmd)}\n")
            
            # Run the command with cleanup
            exec_success, full_output, returncode = self._run_swipl_with_cleanup(cmd, temp_file_path, timeout=60)
            
            if not exec_success:
                return False, full_output
            
            # Print the output
            print("Prolog Output:")
            print("-" * 70)
            print(full_output)
            print("-" * 70)
            
            # Check if the query succeeded based on exit code
            success = returncode == 0
            
            if success:
                print("\n✓ Query evaluation: TRUE")
            else:
                print("\n✗ Query evaluation: FALSE")
            
            return success, full_output
            
        except FileNotFoundError:
            error_msg = "Error: swipl (SWI-Prolog) not found. Please install SWI-Prolog."
            print(error_msg)
            return False, error_msg
        
        except Exception as e:
            error_msg = f"Error executing Prolog query: {e}"
            print(error_msg)
            return False, error_msg
    
    def legality(self, proc_name, actions):
        """
        Perform a legality/executability check: verify if a sequence of actions 
        leads to a legal situation. A situation is legal if all action preconditions 
        are satisfied throughout the execution.
        
        Args:
            proc_name: The name of the procedure to define
            actions: List of actions in execution order
        
        Returns:
            tuple: (success: bool, output: str)
        """
        # Format the action list for Prolog
        action_list_str = '[' + ', '.join(actions) + ']'
        
        print(f"\n{'='*70}")
        print(f"LEGALITY/EXECUTABILITY CHECK")
        print(f"{'='*70}")
        print(f"Model:           {self.model_name}")
        print(f"Procedure:       {proc_name}")
        print(f"Action sequence: {' -> '.join(actions)}")
        print(f"{'='*70}\n")
        
        return self._run_legality_check(proc_name, action_list_str)
    
    def _run_legality_check(self, proc_name, action_list_str):
        """
        Run a legality check by defining a procedure and executing it with indigolog.
        
        Args:
            proc_name: The name of the procedure
            action_list_str: Formatted action list string
        
        Returns:
            tuple: (success: bool, output: str)
        """
        # Create a temporary file with the procedure definition and execution
        with tempfile.NamedTemporaryFile(mode='w', suffix='.pl', delete=False) as temp_file:
            temp_file_path = temp_file.name
            # Define the procedure with the action sequence
            temp_file.write(f":- discontiguous proc/2.\n")
            temp_file.write(f"proc({proc_name}, {action_list_str}).\n\n")
            # Initialize and run the legality check
            temp_file.write(":- initialization(run_legality_check).\n")
            temp_file.write("run_legality_check :- (\n")
            temp_file.write(f"    catch(\n")
            temp_file.write(f"        (indigolog({proc_name}) -> Result = success ; Result = failure),\n")
            temp_file.write(f"        Error,\n")
            temp_file.write(f"        (\n")
            temp_file.write(f"            writeln('RESULT: ERROR - Exception during execution.'),\n")
            temp_file.write(f"            format('ERROR: ~w~n', [Error]),\n")
            temp_file.write(f"            halt(1)\n")
            temp_file.write(f"        )\n")
            temp_file.write(f"    ),\n")
            temp_file.write(f"    (Result = success ->\n")
            temp_file.write("        (writeln('RESULT: SUCCESS - Action sequence is executable.'), halt(0))\n")
            temp_file.write("    ;\n")
            temp_file.write("        (writeln('RESULT: FAILURE - Action sequence not executable.'), halt(1))\n")
            temp_file.write("    )\n")
            temp_file.write("    ).\n")
        
        try:
            # Build the swipl command
            cmd = [
                'swipl',
                '-g', 'true',
                '-t', 'halt',
                self.config_pl,
                self.main_pl,
                temp_file_path
            ]
            
            print("Checking executability of action sequence...")
            print(f"Prolog procedure: proc({proc_name}, {action_list_str}).")
            print(f"Command: {' '.join(cmd)}\n")
            
            # Run the command with cleanup
            exec_success, full_output, returncode = self._run_swipl_with_cleanup(cmd, temp_file_path, timeout=120)
            
            if not exec_success:
                return False, full_output
            
            # Print the output
            print("Prolog Output:")
            print("-" * 70)
            print(full_output)
            print("-" * 70)
            
            # Check if the execution succeeded based on exit code AND output analysis
            # Look for specific failure indicators in the output
            program_failed = "PROGRAM: Program fails" in full_output
            result_success = "RESULT: SUCCESS" in full_output
            result_failure = "RESULT: FAILURE" in full_output
            
            # Determine success: exit code 0, explicit success message, and no program failure
            if result_failure or program_failed:
                success = False
            elif result_success and returncode == 0 and not program_failed:
                success = True
            else:
                success = returncode == 0
            
            if success:
                print("\n✓ Action sequence is EXECUTABLE (legal)")
            else:
                print("\n✗ Action sequence is NOT EXECUTABLE (illegal)")
            
            return success, full_output
            
        except FileNotFoundError:
            error_msg = "Error: swipl (SWI-Prolog) not found. Please install SWI-Prolog."
            print(error_msg)
            return False, error_msg
        
        except Exception as e:
            error_msg = f"Error executing legality check: {e}"
            print(error_msg)
            return False, error_msg
    
    def execute_process(self, controller_number=1):
        """
        Execute the whole BPMN process by automatically selecting a controller.
        
        Args:
            controller_number: The controller number to select (default: 1)
        
        Returns:
            tuple: (success: bool, output: str)
        """
        print(f"\n{'='*70}")
        print(f"PROCESS EXECUTION")
        print(f"{'='*70}")
        print(f"Model:      {self.model_name}")
        print(f"Controller: {controller_number}")
        print(f"{'='*70}\n")
        
        return self._run_process_execution(controller_number)
    
    def _run_process_execution(self, controller_number):
        """
        Run the full process execution by automatically selecting the controller.
        
        Args:
            controller_number: The controller number to select
        
        Returns:
            tuple: (success: bool, output: str)
        """
        # Create a temporary file that automatically selects the controller and captures history
        with tempfile.NamedTemporaryFile(mode='w', suffix='.pl', delete=False) as temp_file:
            temp_file_path = temp_file.name
            # Override the main/0 predicate to automatically select controller
            temp_file.write(":- initialization(run_process).\n")
            temp_file.write("run_process :- (\n")
            temp_file.write(f"    findall(C, proc(control(C), _), LC),\n")
            temp_file.write(f"    length(LC, N),\n")
            temp_file.write(f"    ({controller_number} =< N ->\n")
            temp_file.write(f"        (\n")
            temp_file.write(f"            nth1({controller_number}, LC, C),\n")
            temp_file.write(f"            format('Auto-selecting controller ~d: ~w~n', [{controller_number}, C]),\n")
            temp_file.write(f"            catch(\n")
            temp_file.write(f"                main(control(C)),\n")
            temp_file.write(f"                Error,\n")
            temp_file.write(f"                (\n")
            temp_file.write(f"                    writeln('RESULT: ERROR - Exception during execution.'),\n")
            temp_file.write(f"                    format('ERROR: ~w~n', [Error]),\n")
            temp_file.write(f"                    halt(1)\n")
            temp_file.write(f"                )\n")
            temp_file.write(f"            ),\n")
            # Capture and display the execution history
            temp_file.write(f"            writeln(''),\n")
            temp_file.write(f"            writeln('========================================'),\n")
            temp_file.write(f"            writeln('EXECUTION HISTORY (Action Sequence):'),\n")
            temp_file.write(f"            writeln('========================================'),\n")
            temp_file.write(f"            (progressed_history(H) -> \n")
            temp_file.write(f"                (\n")
            temp_file.write(f"                    length(H, Len),\n")
            temp_file.write(f"                    format('Total actions executed: ~w~n', [Len]),\n")
            temp_file.write(f"                    writeln(''),\n")
            temp_file.write(f"                    writeln('Action sequence (chronological order):'),\n")
            temp_file.write(f"                    reverse(H, RH),\n")
            temp_file.write(f"                    forall(member(A, RH), (\n")
            temp_file.write(f"                        format('  - ~w~n', [A])\n")
            temp_file.write(f"                    ))\n")
            temp_file.write(f"                )\n")
            temp_file.write(f"            ;\n")
            temp_file.write(f"                writeln('  (No history available)')\n")
            temp_file.write(f"            ),\n")
            temp_file.write(f"            writeln('========================================'),\n")
            temp_file.write(f"            writeln(''),\n")
            temp_file.write(f"            writeln('RESULT: SUCCESS - Process execution completed.'),\n")
            temp_file.write(f"            halt(0)\n")
            temp_file.write(f"        )\n")
            temp_file.write(f"    ;\n")
            temp_file.write(f"        (\n")
            temp_file.write(f"            format('ERROR: Controller number ~d not found. Available controllers: ~w~n', [{controller_number}, LC]),\n")
            temp_file.write(f"            writeln('RESULT: FAILURE - Invalid controller number.'),\n")
            temp_file.write(f"            halt(1)\n")
            temp_file.write(f"        )\n")
            temp_file.write(f"    )\n")
            temp_file.write("    ).\n")
        
        process = None
        try:
            # Build the swipl command
            cmd = [
                'swipl',
                '-g', 'true',
                '-t', 'halt',
                self.config_pl,
                self.main_pl,
                temp_file_path
            ]
            
            print(f"Executing BPMN process (controller {controller_number})...")
            print(f"Command: {' '.join(cmd)}\n")
            
            # Run the command with cleanup
            exec_success, full_output, returncode = self._run_swipl_with_cleanup(cmd, temp_file_path, timeout=300)
            
            if not exec_success:
                return False, full_output
            
            # Print the output
            print("Prolog Output:")
            print("-" * 70)
            print(full_output)
            print("-" * 70)
            
            # Extract and display execution history from IndiGolog output
            # Look for the history that IndiGolog prints when program completes
            import re
            history_match = re.search(
                r'PROGRAM: Program has executed to completion!! History done:\s*\n?\s*\[(.*?)\]',
                full_output,
                re.DOTALL
            )
            
            if history_match:
                history_str = history_match.group(1).strip()
                # Split the actions (handle multi-line history)
                # Remove extra whitespace and newlines
                history_str = re.sub(r'\s+', ' ', history_str)
                # Split by comma, but respect parentheses depth
                actions = parse_action_list(history_str)
                # IndiGolog stores history in reverse chronological order, so reverse it
                actions.reverse()
                
                print("\n" + "="*70)
                print("EXTRACTED EXECUTION HISTORY")
                print("="*70)
                print(f"Total actions executed: {len(actions)}")
                print("\nAction sequence (chronological order):")
                for i, action in enumerate(actions, 1):
                    print(f"  {i}. {action}")
                print("="*70)
            else:
                # Fallback: try to extract from our custom output format (if available)
                history_match2 = re.search(
                    r'========================================\s*'
                    r'EXECUTION HISTORY.*?\s*'
                    r'========================================\s*'
                    r'(.*?)'
                    r'========================================',
                    full_output,
                    re.DOTALL
                )
                if history_match2:
                    history_section = history_match2.group(1).strip()
                    print("\n" + "="*70)
                    print("EXTRACTED EXECUTION HISTORY")
                    print("="*70)
                    print(history_section)
                    print("="*70)
            
            # Check if the execution succeeded
            success = returncode == 0
            
            if success:
                print("\n✓ Process execution COMPLETED successfully")
            else:
                print("\n✗ Process execution FAILED")
            
            return success, full_output
            
        except FileNotFoundError:
            error_msg = "Error: swipl (SWI-Prolog) not found. Please install SWI-Prolog."
            print(error_msg)
            return False, error_msg
        
        except Exception as e:
            error_msg = f"Error executing process: {e}"
            print(error_msg)
            return False, error_msg
    
    def conformance_checking(self, history_actions):
        """
        Perform conformance checking: verify if a given execution history 
        conforms to the BPMN process specification using trans_star/4.
        
        Args:
            history_actions: List of actions representing the execution history
        
        Returns:
            tuple: (success: bool, output: str)
        """
        # Format the history list for Prolog
        history_str = '[' + ', '.join(history_actions) + ']'
        
        print(f"\n{'='*70}")
        print(f"CONFORMANCE CHECKING")
        print(f"{'='*70}")
        print(f"Model:           {self.model_name}")
        print(f"History Length:  {len(history_actions)}")
        print(f"{'='*70}")
        print(f"Checking if history conforms to process specification...")
        print(f"{'='*70}\n")
        
        return self._run_conformance_check(history_str)
    
    def _run_conformance_check(self, history_str):
        """
        Run a conformance check using trans_star/4.
        
        Args:
            history_str: Formatted history list string
        
        Returns:
            tuple: (success: bool, output: str)
        """
        # Create a temporary file with the conformance check
        with tempfile.NamedTemporaryFile(mode='w', suffix='.pl', delete=False) as temp_file:
            temp_file_path = temp_file.name
            # Initialize evaluator and run the conformance check
            temp_file.write(":- initialization(check_conformance).\n\n")
            temp_file.write("check_conformance :-\n")
            temp_file.write("    writeln('Initializing evaluator...'),\n")
            temp_file.write("    initialize(evaluator),\n")
            temp_file.write("    writeln('Running conformance check...'),\n")
            temp_file.write("    writeln(''),\n")
            temp_file.write(f"    H = {history_str},\n")
            temp_file.write("    length(H, Len),\n")
            temp_file.write("    format('History to check: ~w actions~n', [Len]),\n")
            temp_file.write("    writeln(''),\n")
            temp_file.write("    statistics(cputime, T0),\n")
            temp_file.write("    statistics(inferences, I0),\n")
            temp_file.write("    (\n")
            temp_file.write("        (proc(sim(bpmn_process), E), trans_star(E, [], _, H)) ->\n")
            temp_file.write("        (\n")
            temp_file.write("            statistics(cputime, T1),\n")
            temp_file.write("            statistics(inferences, I1),\n")
            temp_file.write("            T is T1 - T0,\n")
            temp_file.write("            I is I1 - I0,\n")
            temp_file.write("            writeln(''),\n")
            temp_file.write("            format('~D inferences, ~3f CPU in ~3f seconds~n', [I, T, T]),\n")
            temp_file.write("            writeln(''),\n")
            temp_file.write("            writeln('RESULT: CONFORMANT - History conforms to process specification.'),\n")
            temp_file.write("            halt(0)\n")
            temp_file.write("        )\n")
            temp_file.write("    ;\n")
            temp_file.write("        (\n")
            temp_file.write("            statistics(cputime, T1),\n")
            temp_file.write("            statistics(inferences, I1),\n")
            temp_file.write("            T is T1 - T0,\n")
            temp_file.write("            I is I1 - I0,\n")
            temp_file.write("            writeln(''),\n")
            temp_file.write("            format('~D inferences, ~3f CPU in ~3f seconds~n', [I, T, T]),\n")
            temp_file.write("            writeln(''),\n")
            temp_file.write("            writeln('RESULT: NON-CONFORMANT - History does NOT conform to process specification.'),\n")
            temp_file.write("            halt(1)\n")
            temp_file.write("        )\n")
            temp_file.write("    ).\n")
        
        try:
            # Build the swipl command
            cmd = [
                'swipl',
                '-g', 'true',
                '-t', 'halt',
                self.config_pl,
                self.main_pl,
                temp_file_path
            ]
            
            print("Executing conformance check...")
            print(f"Command: {' '.join(cmd)}\n")
            
            # Run the command with cleanup
            exec_success, full_output, returncode = self._run_swipl_with_cleanup(cmd, temp_file_path, timeout=120)
            
            if not exec_success:
                return False, full_output
            
            # Print the output
            print("Prolog Output:")
            print("-" * 70)
            print(full_output)
            print("-" * 70)
            
            # Check if the conformance check succeeded
            conformant = "RESULT: CONFORMANT" in full_output
            non_conformant = "RESULT: NON-CONFORMANT" in full_output
            
            if conformant and returncode == 0:
                success = True
                print("\n✓ History is CONFORMANT to process specification")
            elif non_conformant or returncode != 0:
                success = False
                print("\n✗ History is NON-CONFORMANT to process specification")
            else:
                success = returncode == 0
            
            return success, full_output
            
        except FileNotFoundError:
            error_msg = "Error: swipl (SWI-Prolog) not found. Please install SWI-Prolog."
            print(error_msg)
            return False, error_msg
        
        except Exception as e:
            error_msg = f"Error executing conformance check: {e}"
            print(error_msg)
            return False, error_msg
    
    def verify_property(self, property_expr, proc_name='property_verification'):
        """
        Perform property verification: execute a reasoning task procedure 
        that verifies a specific property of the BPMN process.
        
        This task modifies the generated Prolog file to replace the placeholder
        with the actual property to verify, then executes the verification.
        
        The procedure uses search/1 to find a trace that satisfies (or violates)
        a property (e.g., finding a situation where a condition holds).
        
        Args:
            property_expr: The property expression to verify (Prolog predicate)
            proc_name: Name of the procedure to execute (default: property_verification)
        
        Returns:
            tuple: (success: bool, output: str)
        """
        print(f"\n{'='*70}")
        print(f"PROPERTY VERIFICATION")
        print(f"{'='*70}")
        print(f"Model:      {self.model_name}")
        print(f"Procedure:  {proc_name}")
        print(f"Property:   {property_expr}")
        print(f"{'='*70}")
        print(f"Executing verification procedure...")
        print(f"{'='*70}\n")
        
        return self._run_property_verification(property_expr, proc_name)
    
    def _run_property_verification(self, property_expr, proc_name):
        """
        Run a property verification using time(do(property_verification, [], H)).
        
        This method:
        1. Reads the generated model file
        2. Replaces the placeholder with the actual property
        3. Executes the verification using time(do(...))
        
        Args:
            property_expr: The property expression to verify
            proc_name: The name of the procedure to execute
        
        Returns:
            tuple: (success: bool, output: str)
        """
        # Read the model file and replace the property placeholder
        model_file = os.path.join(self.model_dir, f'{self.model_name}.pl')
        
        if not os.path.exists(model_file):
            error_msg = f"Model file not found: {model_file}"
            print(error_msg)
            return False, error_msg
        
        # Create a temporary modified version of the model file
        with open(model_file, 'r') as f:
            model_content = f.read()
        
        # Replace the placeholder with the actual property
        # Find the pattern: "true  % REPLACE WITH PROPERTY" and replace it
        placeholder_pattern = r'true\s*% REPLACE WITH PROPERTY'
        replacement = property_expr
        
        modified_content = re.sub(placeholder_pattern, replacement, model_content, flags=re.MULTILINE)
        
        # Check if replacement was successful
        if modified_content == model_content:
            error_msg = "Could not find property placeholder in model file"
            print(error_msg)
            return False, error_msg
        
        # Write modified content to a temporary file
        with tempfile.NamedTemporaryFile(mode='w', suffix='.pl', delete=False) as temp_model_file:
            temp_model_file_path = temp_model_file.name
            temp_model_file.write(modified_content)
        
        # Create a temporary file with the property verification query
        with tempfile.NamedTemporaryFile(mode='w', suffix='.pl', delete=False) as temp_file:
            temp_file_path = temp_file.name
            # Run the property verification using time(do(...))
            temp_file.write(":- initialization(verify_property).\n\n")
            temp_file.write("verify_property :-\n")
            temp_file.write("    initialize(evaluator),\n")
            temp_file.write("    writeln('Executing property verification...'),\n")
            temp_file.write("    writeln(''),\n")
            temp_file.write(f"    format('Calling: time(do(~w, [], H))~n~n', [{proc_name}]),\n")
            temp_file.write("    (\n")
            temp_file.write(f"        time(do({proc_name}, [], H)) ->\n")
            temp_file.write("        (\n")
            temp_file.write("            writeln(''),\n")
            temp_file.write("            length(H, Len),\n")
            temp_file.write("            format('Result history (~w actions): ~w~n', [Len, H]),\n")
            temp_file.write("            writeln(''),\n")
            temp_file.write("            writeln('RESULT: SUCCESS - Property can be satisfied (trace found).'),\n")
            temp_file.write("            halt(0)\n")
            temp_file.write("        )\n")
            temp_file.write("    ;\n")
            temp_file.write("        (\n")
            temp_file.write("            writeln(''),\n")
            temp_file.write("            writeln('RESULT: FAILURE - Property cannot be satisfied (no trace found).'),\n")
            temp_file.write("            halt(1)\n")
            temp_file.write("        )\n")
            temp_file.write("    ).\n")
        
        try:
            # Build the swipl command using the modified model file
            # We need to create a custom main.pl that loads our modified model instead
            with tempfile.NamedTemporaryFile(mode='w', suffix='.pl', delete=False) as temp_main_file:
                temp_main_file_path = temp_main_file.name
                # Create a main file that loads everything except the original model
                temp_main_file.write(":- dir(indigolog, F), consult(F).\n")
                temp_main_file.write(":- dir(eval_bat, F), consult(F).\n\n")
                temp_main_file.write(f":- consult('{temp_model_file_path}').\n\n")
                temp_main_file.write("em_address(localhost, 8000).\n")
                temp_main_file.write("load_devices([simulator]).\n")
                temp_main_file.write("load_device(simulator, Host:Port, [pid(PID)]) :-\n")
                temp_main_file.write("    dir(dev_simulator, File),\n")
                temp_main_file.write("    ARGS = ['-e', 'swipl', '-t', 'start', File, '--host', Host, '--port', Port],\n")
                temp_main_file.write("    logging(info(5, app), \"Command to initialize device simulator: xterm -e ~w\", [ARGS]),\n")
                temp_main_file.write("    process_create(path(xterm), ARGS, [process(PID)]).\n")
            
            cmd = [
                'swipl',
                '-g', 'true',
                '-t', 'halt',
                self.config_pl,
                temp_main_file_path,
                temp_file_path
            ]
            
            print("Executing property verification...")
            print(f"Property: {property_expr}")
            print(f"Command: {' '.join(cmd)}\n")
            
            # Run the command with cleanup
            exec_success, full_output, returncode = self._run_swipl_with_cleanup(cmd, temp_file_path, timeout=120)
            
            # Clean up the temporary files
            try:
                os.unlink(temp_model_file_path)
            except:
                pass
            try:
                os.unlink(temp_main_file_path)
            except:
                pass
            
            if not exec_success:
                return False, full_output
            
            # Print the output
            print("Prolog Output:")
            print("-" * 70)
            print(full_output)
            print("-" * 70)
            
            # Check if the verification succeeded
            verification_success = "RESULT: SUCCESS" in full_output
            verification_failure = "RESULT: FAILURE" in full_output
            
            if verification_success and returncode == 0:
                success = True
                print("\n✓ Property can be satisfied (trace found)")
            elif verification_failure or returncode != 0:
                success = False
                print("\n✗ Property cannot be satisfied (no trace found)")
            else:
                success = returncode == 0
            
            return success, full_output
            
        except FileNotFoundError:
            error_msg = "Error: swipl (SWI-Prolog) not found. Please install SWI-Prolog."
            print(error_msg)
            return False, error_msg
        
        except Exception as e:
            error_msg = f"Error executing property verification: {e}"
            print(error_msg)
            return False, error_msg
        finally:
            # Ensure temporary files are cleaned up
            try:
                os.unlink(temp_model_file_path)
            except:
                pass
            try:
                os.unlink(temp_main_file_path)
            except:
                pass
    
    def interactive_mode(self):
        """
        Start an interactive SWI-Prolog session with the model loaded.
        This allows users to manually execute queries.
        """
        print(f"\n{'='*70}")
        print(f"INTERACTIVE MODE")
        print(f"{'='*70}")
        print(f"Model: {self.model_name}")
        print(f"Loading: {self.config_pl}")
        print(f"         {self.main_pl}")
        print(f"{'='*70}\n")
        print("Starting SWI-Prolog interactive session...")
        print("You can now manually execute queries like:")
        print(f"  ?- eval(fluent_name, [action1, action2], true).")
        print("\nType 'halt.' to exit.\n")
        
        # Build the swipl command for interactive mode
        cmd = [
            'swipl',
            self.config_pl,
            self.main_pl
        ]
        
        try:
            # Run in interactive mode (don't capture output)
            subprocess.run(cmd, cwd=self.indigolog_dir)
        except FileNotFoundError:
            print("Error: swipl (SWI-Prolog) not found. Please install SWI-Prolog.")
        except Exception as e:
            print(f"Error starting interactive session: {e}")


def main():
    """Main entry point for the reasoning script."""
    parser = argparse.ArgumentParser(
        description="Perform reasoning tasks over IndiGolog BPMN translations",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Projection task
  python reason.py job_application projection \\
      --fluent door_open \\
      --actions open,close \\
      --expected true
  
  # Legality/Executability check
  python reason.py job_application legality \\
      --actions "job_needed(1),prepare_application(end,1)"
  
  # Execute the whole process
  python reason.py job_application execute --controller 1
  
  # Conformance checking
  python reason.py job_application conformance \\
      --history "job_needed(1),acquire(1,applicant),prepare_application(start,1)"
  
  # Property verification
  python reason.py job_application verify --proc-name reasoning_task
  
  # Interactive mode
  python reason.py job_application interactive
        """
    )
    
    parser.add_argument(
        'model_name',
        help='Name of the BPMN model (without .bpmn extension)'
    )
    
    subparsers = parser.add_subparsers(dest='task', help='Reasoning task to perform')
    
    # Projection task
    proj_parser = subparsers.add_parser(
        'projection',
        help='Projection task: determine what would be true/false after a sequence of actions'
    )
    proj_parser.add_argument(
        '--fluent',
        required=True,
        help='Name of the fluent to evaluate'
    )
    proj_parser.add_argument(
        '--actions',
        required=True,
        help='Comma-separated list of actions in execution order (e.g., "open,close")'
    )
    proj_parser.add_argument(
        '--expected',
        required=True,
        choices=['true', 'false'],
        help='Expected truth value of the fluent'
    )
    
    # Legality/Executability task
    legal_parser = subparsers.add_parser(
        'legality',
        help='Legality/Executability check: verify if an action sequence is executable'
    )
    legal_parser.add_argument(
        '--proc-name',
        default='test_sequence',
        help='Name for the procedure to define (default: test_sequence)'
    )
    legal_parser.add_argument(
        '--actions',
        required=True,
        help='Comma-separated list of actions in execution order (e.g., "open,close")'
    )
    
    # Execute task
    execute_parser = subparsers.add_parser(
        'execute',
        help='Execute the whole BPMN process by auto-selecting controller'
    )
    execute_parser.add_argument(
        '--controller',
        type=int,
        default=1,
        help='Controller number to select (default: 1)'
    )
    
    # Conformance checking task
    conform_parser = subparsers.add_parser(
        'conformance',
        help='Conformance checking: verify if an execution history conforms to the process'
    )
    conform_parser.add_argument(
        '--history',
        required=True,
        help='Comma-separated list of actions representing the execution history'
    )
    
    # Property verification task
    verify_parser = subparsers.add_parser(
        'verify',
        help='Property verification: execute a reasoning task to verify process properties'
    )
    verify_parser.add_argument(
        '--proc-name',
        default='property_verification',
        help='Name of the verification procedure to execute (default: property_verification)'
    )
    verify_parser.add_argument(
        '--property',
        required=True,
        help='Property expression to verify (e.g., "signed_contract(id), neg(done(application_finalised(id)))")'
    )
    
    # Interactive mode
    subparsers.add_parser(
        'interactive',
        help='Start an interactive SWI-Prolog session with the model loaded'
    )
    
    args = parser.parse_args()
    
    if not args.task:
        parser.print_help()
        sys.exit(1)
    
    try:
        # Initialize the reasoner
        reasoner = IndiGologReasoner(args.model_name)
        
        # Execute the requested task
        if args.task == 'projection':
            # Parse the action list - need to handle commas inside parentheses
            actions = parse_action_list(args.actions)
            
            # Run the projection task
            success, output = reasoner.projection(
                args.fluent,
                actions,
                args.expected
            )
            
            # Exit with appropriate code
            sys.exit(0 if success else 1)
        
        elif args.task == 'legality':
            # Parse the action list - need to handle commas inside parentheses
            actions = parse_action_list(args.actions)
            
            # Run the legality check
            success, output = reasoner.legality(
                args.proc_name,
                actions
            )
            
            # Exit with appropriate code
            sys.exit(0 if success else 1)
        
        elif args.task == 'execute':
            # Run the full process execution
            success, output = reasoner.execute_process(
                args.controller
            )
            
            # Exit with appropriate code
            sys.exit(0 if success else 1)
        
        elif args.task == 'conformance':
            # Parse the history action list
            history_actions = parse_action_list(args.history)
            
            # Run the conformance check
            success, output = reasoner.conformance_checking(history_actions)
            
            # Exit with appropriate code
            sys.exit(0 if success else 1)
        
        elif args.task == 'verify':
            # Run the property verification
            success, output = reasoner.verify_property(args.property, args.proc_name)
            
            # Exit with appropriate code
            sys.exit(0 if success else 1)
        
        elif args.task == 'interactive':
            reasoner.interactive_mode()
    
    except FileNotFoundError as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"Unexpected error: {e}", file=sys.stderr)
        import traceback
        traceback.print_exc()
        sys.exit(1)


if __name__ == '__main__':
    main()
