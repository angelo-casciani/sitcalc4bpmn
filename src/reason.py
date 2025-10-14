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
            if current_action.strip():
                actions.append(current_action.strip())
            current_action = ""
        else:
            current_action += char
    
    if current_action.strip():
        actions.append(current_action.strip())
    
    return actions


class IndiGologReasoner:
    """Handle reasoning tasks over IndiGolog BPMN translations."""
    
    def __init__(self, model_name):
        """Initialize the reasoner with a specific model.
        
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
    
    def _build_swipl_cmd(self, temp_file_path):
        """Build a standard SWI-Prolog command.
        
        Args:
            temp_file_path: Path to the temporary query file
        
        Returns:
            list: Command list for subprocess
        """
        return [
            'swipl',
            '-g', 'true',
            '-t', 'halt',
            self.config_pl,
            self.main_pl,
            temp_file_path
        ]
    
    def _print_task_header(self, title, details):
        """Print a formatted task header.
        
        Args:
            title: Title of the task
            details: Dictionary of detail key-value pairs
        """
        print(f"\n{'='*70}")
        print(f"{title}")
        print(f"{'='*70}")
        for key, value in details.items():
            padding = ' ' * (16 - len(key))
            print(f"{key}:{padding}{value}")
        print(f"{'='*70}\n")
    
    def _print_output_section(self, output):
        """Print formatted Prolog output.
        
        Args:
            output: Output string to print
        """
        print("Prolog Output:")
        print("-" * 70)
        print(output)
        print("-" * 70)
    
    def projection(self, fluent_name, actions, expected_value):
        """Perform a projection task to determine what would be true/false after an action sequence.
        
        Args:
            fluent_name: The name of the fluent to evaluate
            actions: List of actions in normal order (will be reversed for Prolog)
            expected_value: Expected truth value (true or false)
        
        Returns:
            tuple: (success: bool, output: str)
        """
        reversed_actions = list(reversed(actions))
        action_list_str = '[' + ', '.join(reversed_actions) + ']'
        query = f"eval({fluent_name}, {action_list_str}, {expected_value})"
        
        self._print_task_header("PROJECTION TASK", {
            "Model": self.model_name,
            "Fluent": fluent_name,
            "Action sequence": ' -> '.join(actions),
            "Expected value": expected_value,
            "Prolog query": f"?- {query}."
        })
        
        return self._run_prolog_query(query)
    
    def _cleanup_indigolog_processes(self):
        """Kill all IndiGolog-related processes (xterm, swipl, dev_sim, etc.)
        
        This ensures a clean state before running any reasoning task.
        """
        # Kill processes on port 8000 (Environment Manager)
        try:
            result = subprocess.run(
                ['lsof', '-ti', ':8000'],
                capture_output=True,
                text=True,
                timeout=5
            )
            
            if result.stdout.strip():
                pids = result.stdout.strip().split('\n')
                for pid in pids:
                    if pid:
                        try:
                            os.kill(int(pid), signal.SIGKILL)
                        except:
                            pass
        except:
            # Try fuser as fallback
            try:
                subprocess.run(['fuser', '-k', '8000/tcp'], capture_output=True, timeout=5)
            except:
                pass
        
        # Kill xterm processes (used for simulator)
        try:
            subprocess.run(['pkill', '-9', '-f', 'xterm.*dev_sim'], capture_output=True, timeout=5)
        except:
            pass
        
        # Kill dev_sim.pl processes
        try:
            subprocess.run(['pkill', '-9', '-f', 'dev_sim.pl'], capture_output=True, timeout=5)
        except:
            pass
        
        # Kill any stray swipl processes related to IndiGolog
        try:
            subprocess.run(['pkill', '-9', '-f', 'swipl.*indigolog'], capture_output=True, timeout=5)
        except:
            pass
        
        # Wait for port 8000 to actually be free - poll until it's available
        import time
        max_wait = 10.0  # Maximum 10 seconds
        wait_interval = 0.3
        elapsed = 0.0
        
        while elapsed < max_wait:
            try:
                result = subprocess.run(
                    ['lsof', '-ti', ':8000'],
                    capture_output=True,
                    text=True,
                    timeout=2
                )
                if not result.stdout.strip():
                    # Port is free - wait extra time for TCP to fully release
                    time.sleep(3.0)
                    break
            except:
                # Assume port is free if lsof fails
                time.sleep(1.0)
                break
            
            time.sleep(wait_interval)
            elapsed += wait_interval
    
    def _kill_processes_on_port(self, port=8000):
        """Kill any processes using the specified port.
        
        Args:
            port: Port number to check (default: 8000 for Environment Manager)
        
        DEPRECATED: Use _cleanup_indigolog_processes() instead for comprehensive cleanup.
        """
        try:
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
        """Run a SWI-Prolog command with proper process cleanup.
        
        Args:
            cmd: Command list to execute
            temp_file_path: Path to temporary file (for cleanup)
            timeout: Timeout in seconds (default: 300/5 minutes)
        
        Returns:
            tuple: (success: bool, output: str, returncode: int)
        """
        process = None
        try:
            # Comprehensive cleanup before starting
            self._cleanup_indigolog_processes()
            
            process = subprocess.Popen(
                cmd,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True,
                cwd=self.indigolog_dir,
                preexec_fn=os.setsid if hasattr(os, 'setsid') else None
            )
            
            try:
                stdout, stderr = process.communicate(timeout=timeout)
                full_output = stdout + stderr
                returncode = process.returncode
                return True, full_output, returncode
            except subprocess.TimeoutExpired:
                print(f"WARNING: Process timed out after {timeout} seconds. Terminating...")
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
            if process is not None:
                try:
                    if process.poll() is None:
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
            
            # Comprehensive cleanup after execution
            self._cleanup_indigolog_processes()
            
            try:
                os.unlink(temp_file_path)
            except:
                pass
    
    def _run_prolog_query(self, query):
        """Run a Prolog query using swipl.
        
        Args:
            query: The Prolog query to execute
        
        Returns:
            tuple: (success: bool, output: str)
        """
        with tempfile.NamedTemporaryFile(mode='w', suffix='.pl', delete=False) as temp_file:
            temp_file_path = temp_file.name
            temp_file.write(":- initialization(run_query).\n\n")
            temp_file.write("run_query :-\n")
            temp_file.write("    writeln('Initializing evaluator...'),\n")
            temp_file.write("    initialize(evaluator),\n")
            temp_file.write("    writeln('Running projection query...'),\n")
            temp_file.write("    writeln(''),\n")
            temp_file.write("    statistics(cputime, T0),\n")
            temp_file.write("    statistics(inferences, I0),\n")
            temp_file.write("    (\n")
            temp_file.write(f"        {query} ->\n")
            temp_file.write("        (\n")
            temp_file.write("            statistics(cputime, T1),\n")
            temp_file.write("            statistics(inferences, I1),\n")
            temp_file.write("            T is T1 - T0,\n")
            temp_file.write("            I is I1 - I0,\n")
            temp_file.write("            writeln(''),\n")
            temp_file.write("            format('~D inferences, ~3f CPU in ~3f seconds~n', [I, T, T]),\n")
            temp_file.write("            writeln(''),\n")
            temp_file.write("            writeln('RESULT: SUCCESS - Query succeeded.'),\n")
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
            temp_file.write("            writeln('RESULT: FAILURE - Query failed.'),\n")
            temp_file.write("            halt(1)\n")
            temp_file.write("        )\n")
            temp_file.write("    ).\n")
        
        try:
            cmd = self._build_swipl_cmd(temp_file_path)
            
            print("Executing Prolog query...")
            print(f"Command: {' '.join(cmd)}\n")
            
            exec_success, full_output, returncode = self._run_swipl_with_cleanup(cmd, temp_file_path, timeout=60)
            
            if not exec_success:
                return False, full_output
            
            self._print_output_section(full_output)
            
            success = returncode == 0
            
            # Append query result to output for UI extraction
            if success:
                status_line = "\n✓ Query evaluation: TRUE"
            else:
                status_line = "\n✗ Query evaluation: FALSE"
            
            print(status_line)
            full_output += status_line
            
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
        """Perform a legality/executability check on an action sequence.
        
        Args:
            proc_name: The name of the procedure to define
            actions: List of actions in execution order
        
        Returns:
            tuple: (success: bool, output: str)
        """
        action_list_str = '[' + ', '.join(actions) + ']'
        
        self._print_task_header("LEGALITY/EXECUTABILITY CHECK", {
            "Model": self.model_name,
            "Procedure": proc_name,
            "Action sequence": ' -> '.join(actions)
        })
        
        return self._run_legality_check(proc_name, action_list_str)
    
    def _run_legality_check(self, proc_name, action_list_str):
        """Run a legality check by defining a procedure and executing it with indigolog.
        
        Args:
            proc_name: The name of the procedure
            action_list_str: Formatted action list string
        
        Returns:
            tuple: (success: bool, output: str)
        """
        with tempfile.NamedTemporaryFile(mode='w', suffix='.pl', delete=False) as temp_file:
            temp_file_path = temp_file.name
            temp_file.write(f":- discontiguous proc/2.\n")
            temp_file.write(f"proc({proc_name}, {action_list_str}).\n\n")
            temp_file.write(":- initialization(run_legality_check).\n\n")
            temp_file.write("run_legality_check :-\n")
            temp_file.write("    writeln('Running legality check...'),\n")
            temp_file.write("    writeln(''),\n")
            temp_file.write("    statistics(cputime, T0),\n")
            temp_file.write("    statistics(inferences, I0),\n")
            temp_file.write("    (\n")
            temp_file.write(f"        catch(\n")
            temp_file.write(f"            (indigolog({proc_name}) -> Result = success ; Result = failure),\n")
            temp_file.write(f"            Error,\n")
            temp_file.write(f"            (\n")
            temp_file.write("                statistics(cputime, T1),\n")
            temp_file.write("                statistics(inferences, I1),\n")
            temp_file.write("                T is T1 - T0,\n")
            temp_file.write("                I is I1 - I0,\n")
            temp_file.write("                writeln(''),\n")
            temp_file.write("                format('~D inferences, ~3f CPU in ~3f seconds~n', [I, T, T]),\n")
            temp_file.write("                writeln(''),\n")
            temp_file.write(f"                writeln('RESULT: ERROR - Exception during execution.'),\n")
            temp_file.write(f"                format('ERROR: ~w~n', [Error]),\n")
            temp_file.write("                writeln('Forcing cleanup after exception...'),\n")
            temp_file.write("                catch(finalize(indigolog), _, true),\n")  # Force IndiGolog cleanup even on exception
            temp_file.write(f"                Result = error\n")
            temp_file.write(f"            )\n")
            temp_file.write(f"        ),\n")
            temp_file.write(f"        (Result = success ->\n")
            temp_file.write("            (\n")
            temp_file.write("                statistics(cputime, T1),\n")
            temp_file.write("                statistics(inferences, I1),\n")
            temp_file.write("                T is T1 - T0,\n")
            temp_file.write("                I is I1 - I0,\n")
            temp_file.write("                writeln(''),\n")
            temp_file.write("                format('~D inferences, ~3f CPU in ~3f seconds~n', [I, T, T]),\n")
            temp_file.write("                writeln(''),\n")
            temp_file.write("                writeln('RESULT: SUCCESS - Action sequence is executable.'),\n")
            temp_file.write("                halt(0)\n")
            temp_file.write("            )\n")
            temp_file.write("        ; Result = error ->\n")
            temp_file.write("            halt(1)\n")
            temp_file.write("        ;\n")
            temp_file.write("            (\n")
            temp_file.write("                statistics(cputime, T1),\n")
            temp_file.write("                statistics(inferences, I1),\n")
            temp_file.write("                T is T1 - T0,\n")
            temp_file.write("                I is I1 - I0,\n")
            temp_file.write("                writeln(''),\n")
            temp_file.write("                format('~D inferences, ~3f CPU in ~3f seconds~n', [I, T, T]),\n")
            temp_file.write("                writeln(''),\n")
            temp_file.write("                writeln('RESULT: FAILURE - Action sequence not executable.'),\n")
            temp_file.write("                halt(1)\n")
            temp_file.write("            )\n")
            temp_file.write("        )\n")
            temp_file.write("    ).\n")
        
        try:
            cmd = self._build_swipl_cmd(temp_file_path)
            
            print("Checking executability of action sequence...")
            print(f"Prolog procedure: proc({proc_name}, {action_list_str}).")
            print(f"Command: {' '.join(cmd)}\n")
            
            exec_success, full_output, returncode = self._run_swipl_with_cleanup(cmd, temp_file_path, timeout=120)
            
            if not exec_success:
                return False, full_output
            
            self._print_output_section(full_output)
            
            program_failed = "PROGRAM: Program fails" in full_output
            result_success = "RESULT: SUCCESS" in full_output
            result_failure = "RESULT: FAILURE" in full_output
            
            if result_failure or program_failed:
                success = False
            elif result_success and returncode == 0 and not program_failed:
                success = True
            else:
                success = returncode == 0
        
            if success:
                status_line = "\n✓ Action sequence is EXECUTABLE (legal)"
            else:
                status_line = "\n✗ Action sequence is NOT EXECUTABLE (illegal)"
            
            print(status_line)
            full_output += status_line
            
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
        """Execute the whole BPMN process by automatically selecting a controller.
        
        Args:
            controller_number: The controller number to select (default: 1)
        
        Returns:
            tuple: (success: bool, output: str)
        """
        self._print_task_header("PROCESS EXECUTION", {
            "Model": self.model_name,
            "Controller": str(controller_number)
        })
        
        return self._run_process_execution(controller_number)
    
    def _run_process_execution(self, controller_number):
        """Run the full process execution by automatically selecting the controller.
        
        Args:
            controller_number: The controller number to select
        
        Returns:
            tuple: (success: bool, output: str)
        """
        with tempfile.NamedTemporaryFile(mode='w', suffix='.pl', delete=False) as temp_file:
            temp_file_path = temp_file.name
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
            cmd = self._build_swipl_cmd(temp_file_path)
            
            print(f"Executing BPMN process (controller {controller_number})...")
            print(f"Command: {' '.join(cmd)}\n")
            
            exec_success, full_output, returncode = self._run_swipl_with_cleanup(cmd, temp_file_path, timeout=300)
            
            if not exec_success:
                return False, full_output
            
            self._print_output_section(full_output)
            
            import re
            history_match = re.search(
                r'PROGRAM: Program has executed to completion!! History done:\s*\n?\s*\[(.*?)\]',
                full_output,
                re.DOTALL
            )
            
            extracted_history_text = ""
            if history_match:
                history_str = history_match.group(1).strip()
                history_str = re.sub(r'\s+', ' ', history_str)
                actions = parse_action_list(history_str)
                actions.reverse()
                
                extracted_history_text = "\n" + "="*70 + "\n"
                extracted_history_text += "EXTRACTED EXECUTION HISTORY\n"
                extracted_history_text += "="*70 + "\n"
                extracted_history_text += f"Total actions executed: {len(actions)}\n"
                extracted_history_text += "\nAction sequence (chronological order):\n"
                for i, action in enumerate(actions, 1):
                    extracted_history_text += f"  {i}. {action}\n"
                extracted_history_text += "="*70
                
                print(extracted_history_text)
            else:
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
                    extracted_history_text = "\n" + "="*70 + "\n"
                    extracted_history_text += "EXTRACTED EXECUTION HISTORY\n"
                    extracted_history_text += "="*70 + "\n"
                    extracted_history_text += history_section + "\n"
                    extracted_history_text += "="*70
                    
                    print(extracted_history_text)
            
            # Append extracted history to full_output so it's available to the UI
            if extracted_history_text:
                full_output += "\n" + extracted_history_text
            
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
        """Perform conformance checking using trans_star/4.
        
        Args:
            history_actions: List of actions representing the execution history
        
        Returns:
            tuple: (success: bool, output: str)
        """
        history_str = '[' + ', '.join(history_actions) + ']'
        
        self._print_task_header("CONFORMANCE CHECKING", {
            "Model": self.model_name,
            "History Length": str(len(history_actions))
        })
        print("Checking if history conforms to process specification...\n")
        
        return self._run_conformance_check(history_str)
    
    def _run_conformance_check(self, history_str):
        """Run a conformance check using trans_star/4.
        
        Args:
            history_str: Formatted history list string
        
        Returns:
            tuple: (success: bool, output: str)
        """
        with tempfile.NamedTemporaryFile(mode='w', suffix='.pl', delete=False) as temp_file:
            temp_file_path = temp_file.name
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
            cmd = self._build_swipl_cmd(temp_file_path)
            
            print("Executing conformance check...")
            print(f"Command: {' '.join(cmd)}\n")
            
            exec_success, full_output, returncode = self._run_swipl_with_cleanup(cmd, temp_file_path, timeout=120)
            
            if not exec_success:
                return False, full_output
            
            self._print_output_section(full_output)
            
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
        """Perform property verification by executing a reasoning task procedure.
        
        Args:
            property_expr: The property expression to verify (Prolog predicate)
            proc_name: Name of the procedure to execute (default: property_verification)
        
        Returns:
            tuple: (success: bool, output: str)
        """
        self._print_task_header("PROPERTY VERIFICATION", {
            "Model": self.model_name,
            "Procedure": proc_name,
            "Property": property_expr
        })
        print("Executing verification procedure...\n")
        
        return self._run_property_verification(property_expr, proc_name)
    
    def _run_property_verification(self, property_expr, proc_name):
        """Run a property verification using time(do(...)).
        
        Args:
            property_expr: The property expression to verify
            proc_name: The name of the procedure to execute
        
        Returns:
            tuple: (success: bool, output: str)
        """
        model_file = os.path.join(self.model_dir, f'{self.model_name}.pl')
        
        if not os.path.exists(model_file):
            error_msg = f"Model file not found: {model_file}"
            print(error_msg)
            return False, error_msg
        
        with open(model_file, 'r') as f:
            model_content = f.read()
        
        placeholder_pattern = r'true\s*% REPLACE WITH PROPERTY'
        replacement = property_expr
        
        modified_content = re.sub(placeholder_pattern, replacement, model_content, flags=re.MULTILINE)
        
        if modified_content == model_content:
            error_msg = "Could not find property placeholder in model file"
            print(error_msg)
            return False, error_msg
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.pl', delete=False) as temp_model_file:
            temp_model_file_path = temp_model_file.name
            temp_model_file.write(modified_content)
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.pl', delete=False) as temp_file:
            temp_file_path = temp_file.name
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
            with tempfile.NamedTemporaryFile(mode='w', suffix='.pl', delete=False) as temp_main_file:
                temp_main_file_path = temp_main_file.name
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
            
            exec_success, full_output, returncode = self._run_swipl_with_cleanup(cmd, temp_file_path, timeout=120)
            
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
            
            self._print_output_section(full_output)
            
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
            try:
                os.unlink(temp_model_file_path)
            except:
                pass
            try:
                os.unlink(temp_main_file_path)
            except:
                pass
    
    def interactive_mode(self):
        """Start an interactive SWI-Prolog session with the model loaded."""
        self._print_task_header("INTERACTIVE MODE", {
            "Model": self.model_name,
            "Loading": f"{self.config_pl}\n                {self.main_pl}"
        })
        print("Starting SWI-Prolog interactive session...")
        print("You can now manually execute queries like:")
        print(f"  ?- eval(fluent_name, [action1, action2], true).")
        print("\nType 'halt.' to exit.\n")
        
        cmd = [
            'swipl',
            self.config_pl,
            self.main_pl
        ]
        
        try:
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
        help='Property verification: verify properties over the whole process execution'
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
        reasoner = IndiGologReasoner(args.model_name)
        
        if args.task == 'projection':
            actions = parse_action_list(args.actions)
            success, output = reasoner.projection(args.fluent, actions, args.expected)
            sys.exit(0 if success else 1)
        
        elif args.task == 'legality':
            actions = parse_action_list(args.actions)
            success, output = reasoner.legality(args.proc_name, actions)
            sys.exit(0 if success else 1)
        
        elif args.task == 'execute':
            success, output = reasoner.execute_process(args.controller)
            sys.exit(0 if success else 1)
        
        elif args.task == 'conformance':
            history_actions = parse_action_list(args.history)
            success, output = reasoner.conformance_checking(history_actions)
            sys.exit(0 if success else 1)
        
        elif args.task == 'verify':
            success, output = reasoner.verify_property(args.property, args.proc_name)
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
