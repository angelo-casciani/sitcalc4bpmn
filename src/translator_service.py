"""
Translation Service Module

This module provides a clean API for BPMN to IndiGolog translation,
decoupling the UI from the computation logic.
"""

import os
import sys
from pathlib import Path
from bpmn_parser import BPMNParser
from prolog_translator import PrologTranslator


# Template for the main.pl file, with a placeholder for the generated file's name.
MAIN_PL_TEMPLATE = """/* Job Application process MAIN file 

    This file is the main file for the job application process. It
    loads the necessary files and starts the application.

    The application is a process simulator that is controlled by
    an INDIGOLOG program. A TCL/TK interface can be used to issue
    exogenous events/actions.
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONSULT INDIGOLOG FRAMEWORK
%
%    Configuration files
%    Interpreter
%    Environment manager
%    Evaluation engine/Projector
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dir(indigolog, F), consult(F).
:- dir(eval_bat, F), consult(F).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONSULT APPLICATION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- [{prolog_basename}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SPECIFY ADDRESS OF ENVIRONMENT MANAGER
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Any port available would be ok for the EM.
em_address(localhost, 8000).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ENVIRONMENTS/DEVICES TO LOAD
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

load_devices([simulator]).

% start env_sim.pl tcl/tk interaction interface
load_device(simulator, Host:Port, [pid(PID)]) :-
    % root_indigolog(Dir),
    dir(dev_simulator, File),
    ARGS = ['-e', 'swipl', '-t', 'start', File, '--host', Host, '--port', Port],
    logging(info(5, app), "Command to initialize device simulator: xterm -e ~w", [ARGS]),
    process_create(path(xterm), ARGS, [process(PID)]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HOW TO EXECUTE ACTIONS: Environment + low-level Code
%        how_to_execute(Action, Environment, Code)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
how_to_execute(Action, simulator, sense(Action)) :-
    sensing_action(Action, _).
how_to_execute(Action, simulator, Action) :-
    \+ sensing_action(Action, _).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   EXOGENOUS ACTION AND SENSING OUTCOME TRANSLATION
%
%          translate_exog(Code, Action)
%          translate_sensing(Action, Outcome, Value)
%
% OBS: If not present, then the translation is 1-1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
translate_exog(ActionCode, Action) :- actionNum(Action, ActionCode), !.
translate_exog(A, A).
translate_sensing(_, SR, SR).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MAIN PREDICATE - evaluate this to run demo
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% main/0: Gets INDIGOLOG to evaluate a chosen mainControl procedure
main :-
    findall(C, proc(control(C), _), LC),
    length(LC, N),
    repeat,
    format('Controllers available: ~w\\n', [LC]),
    forall((between(1, N, I), nth1(I, LC, C)),
        format('~d. ~w\\n', [I, C])),
    nl, nl,
    write('Select controller: '),
	read(NC), nl,
    number(NC),
    nth1(NC, LC, C),
	format('Executing controller: *~w*\\n', [C]), !,
    main(control(C)).

main(C) :- assert(control(C)), indigolog(C).


:- set_option(log_level, 2).
:- set_option(log_level, em(1)).
:- set_option(wait_step, 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
"""


class TranslatorService:
    """Service class for BPMN to IndiGolog translation."""
    
    def __init__(self):
        """Initialize the translator service."""
        self.script_dir = Path(__file__).parent.resolve()
        self.project_root = self.script_dir.parent
        self.models_dir = self.project_root / 'models'
        self.pl_models_dir = self.project_root / 'pl_models'
        
        # Ensure directories exist
        self.models_dir.mkdir(exist_ok=True)
        self.pl_models_dir.mkdir(exist_ok=True)
    
    def translate_bpmn_file(self, bpmn_file_path, model_name=None):
        """
        Translate a BPMN file to IndiGolog.
        
        Args:
            bpmn_file_path: Path to the BPMN file to translate
            model_name: Name for the model (if None, derived from filename)
        
        Returns:
            tuple: (success: bool, message: str, translated_pl_path: str, prolog_code: str)
        """
        try:
            bpmn_path = Path(bpmn_file_path)
            
            # Validate file exists
            if not bpmn_path.exists():
                return False, f"BPMN file not found: {bpmn_file_path}", None, None
            
            # Derive model name if not provided
            if model_name is None:
                model_name = bpmn_path.stem
            
            # Create output directory
            model_output_dir = self.pl_models_dir / model_name
            model_output_dir.mkdir(exist_ok=True)
            
            # Parse BPMN
            try:
                parser = BPMNParser(str(bpmn_path))
            except Exception as e:
                return False, f"Error parsing BPMN file: {str(e)}", None, None
            
            # Translate to Prolog
            translator = PrologTranslator(parser)
            prolog_code = translator.translate()
            
            # Write translated process file
            translated_pl_path = model_output_dir / f'{model_name}.pl'
            try:
                with open(translated_pl_path, 'w') as f:
                    f.write(prolog_code)
            except Exception as e:
                return False, f"Error writing translated file: {str(e)}", None, None
            
            # Generate main.pl file
            main_pl_path = model_output_dir / 'main.pl'
            main_pl_content = MAIN_PL_TEMPLATE.format(prolog_basename=model_name)
            try:
                with open(main_pl_path, 'w') as f:
                    f.write(main_pl_content)
            except Exception as e:
                return False, f"Error writing main.pl file: {str(e)}", None, None
            
            success_msg = (
                f"✓ Translation successful!\n"
                f"Model name: {model_name}\n"
                f"Translated file: {translated_pl_path}\n"
                f"Main file: {main_pl_path}"
            )
            
            return True, success_msg, str(translated_pl_path), prolog_code
            
        except Exception as e:
            return False, f"Unexpected error during translation: {str(e)}", None, None
    
    def get_translated_prolog(self, model_name):
        """
        Read the translated Prolog code for a model.
        
        Args:
            model_name: Name of the model
        
        Returns:
            tuple: (success: bool, content: str)
        """
        try:
            translated_pl_path = self.pl_models_dir / model_name / f'{model_name}.pl'
            
            if not translated_pl_path.exists():
                return False, f"Translated file not found for model: {model_name}"
            
            with open(translated_pl_path, 'r') as f:
                content = f.read()
            
            return True, content
            
        except Exception as e:
            return False, f"Error reading translated file: {str(e)}"
    
    def list_available_models(self):
        """
        List all available translated models.
        
        Returns:
            list: List of model names
        """
        try:
            models = []
            for item in self.pl_models_dir.iterdir():
                if item.is_dir():
                    # Check if it has the required files
                    pl_file = item / f'{item.name}.pl'
                    main_file = item / 'main.pl'
                    if pl_file.exists() and main_file.exists():
                        models.append(item.name)
            return sorted(models)
        except Exception:
            return []
