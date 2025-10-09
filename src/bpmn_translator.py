import argparse
import os
import sys
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

def main():
    """
    Main function to run the BPMN to IndiGolog translation with a specific directory structure.
    """
    arg_parser = argparse.ArgumentParser(
        description="Translate a BPMN model from the '../models' directory to an IndiGolog process in the '../pl_models' directory."
    )
    arg_parser.add_argument(
        "bpmn_name",
        help="The base name of the BPMN model located in the '../models' directory (e.g., 'job_application'). Do not include the .bpmn extension."
    )
    args = arg_parser.parse_args()

    # --- 1. Define File Paths based on the new structure ---
    try:
        script_dir = os.path.dirname(os.path.realpath(__file__))
        bpmn_name = args.bpmn_name

        bpmn_file_path = os.path.join(script_dir, '..', 'models', f"{bpmn_name}.bpmn")
        
        pl_model_dir = os.path.join(script_dir, '..', 'pl_models', bpmn_name)
        os.makedirs(pl_model_dir, exist_ok=True)
        
        translated_pl_path = os.path.join(pl_model_dir, f'{bpmn_name}.pl')
        main_pl_path = os.path.join(pl_model_dir, 'main.pl')
    except Exception as e:
        print(f"Error setting up file paths: {e}")
        sys.exit(1)

    # --- 2. Translate the BPMN file ---
    print(f"Input BPMN file: {bpmn_file_path}")
    try:
        parser = BPMNParser(bpmn_file_path)
    except FileNotFoundError:
        print(f"Error: Input file not found at '{bpmn_file_path}'. Please ensure it exists in the 'models' directory.")
        sys.exit(1)
    except Exception as e:
        print(f"An error occurred during parsing: {e}")
        sys.exit(1)

    print("Translating BPMN process to IndiGolog...")
    translator = PrologTranslator(parser)
    prolog_code = translator.translate()

    try:
        with open(translated_pl_path, 'w') as f:
            f.write(prolog_code)
        print(f"Successfully generated translated process file: {translated_pl_path}")
    except Exception as e:
        print(f"Error writing to output process file '{translated_pl_path}': {e}")
        sys.exit(1)

    # --- 3. Generate the main.pl file ---
    print(f"Generating main Prolog file: {main_pl_path}")
    try:
        # The prolog_basename is now simply the bpmn_name
        main_pl_content = MAIN_PL_TEMPLATE.format(prolog_basename=bpmn_name)

        with open(main_pl_path, 'w') as f:
            f.write(main_pl_content)
        print(f"Successfully generated main Prolog file: {main_pl_path}")
    except Exception as e:
        print(f"Error writing to main Prolog file '{main_pl_path}': {e}")

    print("\nTranslation complete.")


if __name__ == "__main__":
    main()