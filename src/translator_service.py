from pathlib import Path
from bpmn_parser import BPMNParser
from prolog_translator import PrologTranslator
from prolog_templates import *


class TranslatorService:
    def __init__(self):
        self.script_dir = Path(__file__).parent.resolve()
        self.project_root = self.script_dir.parent
        self.bpmn_dir = self.project_root / 'bpmn'
        self.pl_models_dir = self.project_root / 'pl_models'
        self.bpmn_dir.mkdir(exist_ok=True)
        self.pl_models_dir.mkdir(exist_ok=True)
    
    def translate_bpmn_file(self, bpmn_file_path, model_name=None):
        try:
            bpmn_path = Path(bpmn_file_path)
            if not bpmn_path.exists():
                return False, f"BPMN file not found: {bpmn_file_path}", None, None
            
            if model_name is None:
                model_name = bpmn_path.stem
            
            model_output_dir = self.pl_models_dir / model_name
            model_output_dir.mkdir(exist_ok=True)
            try:
                parser = BPMNParser(str(bpmn_path))
            except Exception as e:
                return False, f"Error parsing BPMN file: {str(e)}", None, None
            
            translator = PrologTranslator(parser)
            prolog_code = translator.translate()
            translated_pl_path = model_output_dir / f'{model_name}.pl'
            try:
                with open(translated_pl_path, 'w') as f:
                    f.write(prolog_code)
            except Exception as e:
                return False, f"Error writing translated file: {str(e)}", None, None
            
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
        try:
            models = []
            for item in self.pl_models_dir.iterdir():
                if item.is_dir():
                    pl_file = item / f'{item.name}.pl'
                    main_file = item / 'main.pl'
                    if pl_file.exists() and main_file.exists():
                        models.append(item.name)
            return sorted(models)
        except Exception:
            return []