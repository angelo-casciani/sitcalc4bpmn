import argparse
import os
import sys
from bpmn_parser import BPMNParser
from prolog_translator import PrologTranslator
from prolog_templates import *


def main():
    arg_parser = argparse.ArgumentParser(
        description="Translate a BPMN model from the '../bpmn' directory to an IndiGolog process in the '../pl_models' directory."
    )
    arg_parser.add_argument(
        "bpmn_name",
        help="The base name of the BPMN model located in the '../bpmn' directory (e.g., 'job_application'). Do not include the .bpmn extension."
    )
    args = arg_parser.parse_args()

    # --- 1. Define File Paths based on the new structure ---
    try:
        script_dir = os.path.dirname(os.path.realpath(__file__))
        bpmn_name = args.bpmn_name

        bpmn_dir = os.path.join(script_dir, '..', 'bpmn')
        possible_extensions = ['.bpmn', '.bpmn2.xml', '.xml']
        bpmn_file_path = None
        
        for ext in possible_extensions:
            candidate_path = os.path.join(bpmn_dir, f"{bpmn_name}{ext}")
            if os.path.exists(candidate_path):
                bpmn_file_path = candidate_path
                break
        
        if bpmn_file_path is None:
            print(f"Error: No BPMN file found for '{bpmn_name}' with extensions {possible_extensions}")
            print(f"Searched in: {bpmn_dir}")
            sys.exit(1)
        
        pl_model_dir = os.path.join(script_dir, '..', 'pl_models', bpmn_name)
        os.makedirs(pl_model_dir, exist_ok=True)
        
        # Extract just the base filename (last component) for the .pl file
        base_name = os.path.basename(bpmn_name)
        translated_pl_path = os.path.join(pl_model_dir, f'{base_name}.pl')
        main_pl_path = os.path.join(pl_model_dir, 'main.pl')
    except Exception as e:
        print(f"Error setting up file paths: {e}")
        sys.exit(1)

    # --- 2. Translate the BPMN file ---
    print(f"Input BPMN file: {bpmn_file_path}")
    try:
        parser = BPMNParser(bpmn_file_path)
    except FileNotFoundError:
        print(f"Error: Input file not found at '{bpmn_file_path}'. Please ensure it exists in the 'bpmn' directory.")
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
        main_pl_content = MAIN_PL_TEMPLATE.format(prolog_basename=bpmn_name)
        with open(main_pl_path, 'w') as f:
            f.write(main_pl_content)
        print(f"Successfully generated main Prolog file: {main_pl_path}")
    except Exception as e:
        print(f"Error writing to main Prolog file '{main_pl_path}': {e}")

    print("\nTranslation complete.")


if __name__ == "__main__":
    main()