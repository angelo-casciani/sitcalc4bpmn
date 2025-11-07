import argparse
import os
import sys
import csv
from typing import List, Dict, Optional

eval_src_dir = os.path.dirname(os.path.abspath(__file__))
main_src_dir = os.path.abspath(os.path.join(eval_src_dir, '..', '..', 'src'))
sys.path.insert(0, eval_src_dir)
sys.path.insert(0, main_src_dir)

from metrics_collector import MetricsCollector, ReasoningMetrics
from translator_service import TranslatorService
from reasoning_service import ReasoningService


class BPMNEvaluator:
    """Evaluator for BPMN legality and conformance."""
    def __init__(self, dataset_dir: str, output_dir: str):
        self.dataset_dir = dataset_dir
        self.output_dir = output_dir
        self.results = []
        os.makedirs(os.path.join(output_dir, 'results'), exist_ok=True)
        
        # Set up evaluation_temp directory for generated models
        self.project_root = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..'))
        self.pl_models_dir = os.path.join(self.project_root, 'pl_models', 'evaluation_temp')
        os.makedirs(self.pl_models_dir, exist_ok=True)
        
        # Initialize translator with evaluation_temp directory
        self.translator = TranslatorService(output_dir=self.pl_models_dir)
    
    def translate_bpmn_model(self, bpmn_path: str, model_name: str) -> str:
        try:
            model_id = model_name.replace('.bpmn', '')
            eval_model_name = f'{model_id}'
            success, message, _, _ = self.translator.translate_bpmn_file(bpmn_path, eval_model_name)
            
            if not success:
                print(f"  Translation failed: {message}")
                return None
            
            return eval_model_name
        except Exception as e:
            print(f"  Error translating {model_name}: {e}")
            import traceback
            traceback.print_exc()
            return None
    
    def run_legality_task(self, reasoner: ReasoningService, sample: Dict) -> Optional[ReasoningMetrics]:
        try:
            from reason import parse_action_list
            actions_str = ', '.join(sample['actions'])
            actions = parse_action_list(actions_str)
            # Call reasoner method directly to get raw output
            success, raw_output = reasoner.reasoner.legality('legality_check', actions)
            metrics = MetricsCollector.parse_prolog_output(raw_output, success)
            return metrics
        
        except Exception as e:
            print(f"    Error in legality task: {e}")
            return None
    
    def run_conformance_task(self, reasoner: ReasoningService, sample: Dict) -> Optional[ReasoningMetrics]:
        try:
            from reason import parse_action_list
            history_str = ', '.join(sample['actions'])
            history_actions = parse_action_list(history_str)
            reversed_history = list(reversed(history_actions))
            # Call reasoner method directly to get raw output
            success, raw_output = reasoner.reasoner.conformance_checking(reversed_history)
            metrics = MetricsCollector.parse_prolog_output(raw_output, success)
            return metrics
        
        except Exception as e:
            print(f"    Error in conformance task: {e}")
            return None
    
    def run_projection_task(self, reasoner: ReasoningService, sample: Dict) -> Optional[ReasoningMetrics]:
        try:
            from reason import parse_action_list
            actions_str = ', '.join(sample['actions'])
            actions = parse_action_list(actions_str)
            fluent_name = sample['property']
            expected_value = 'true' if sample['expected_result'] else 'false'
            
            # Call reasoner method directly to get raw output
            success, raw_output = reasoner.reasoner.projection(fluent_name, actions, expected_value)
            metrics = MetricsCollector.parse_prolog_output(raw_output, success)
            return metrics
        
        except Exception as e:
            print(f"    Error in projection task: {e}")
            return None
    
    def run_verification_task(self, reasoner: ReasoningService, sample: Dict) -> Optional[ReasoningMetrics]:
        try:
            import re
            property_str = sample['property']
            proc_name = 'property_verification'
            
            # Parse property expression (same logic as in reasoning_service.py)
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
            
            if current_condition.strip():
                conditions.append(current_condition.strip())
            
            if len(conditions) == 0:
                return None
            elif len(conditions) == 1:
                property_expr = conditions[0]
            else:
                property_expr = conditions[-1]
                for cond in reversed(conditions[:-1]):
                    property_expr = f"and({cond}, {property_expr})"
            
            # Call reasoner method directly to get raw output
            success, raw_output = reasoner.reasoner.verify_property(property_expr, proc_name)
            metrics = MetricsCollector.parse_prolog_output(raw_output, success)
            return metrics
        
        except Exception as e:
            print(f"    Error in verification task: {e}")
            return None

    
    def load_samples_from_csv(self, csv_path: str) -> Dict[str, List[Dict]]:
        samples_by_model = {}
        with open(csv_path, 'r', newline='') as f:
            reader = csv.DictReader(f)
            for row in reader:
                task_type = row['task_type']
                if task_type not in ['legality', 'conformance', 'projection', 'verification']:
                    continue    
                
                # Parse actions (empty for verification tasks)
                actions = [a.strip() for a in row['actions'].split(';') if a.strip()]
                expected_result = row['expected_result'].lower() == 'true'
                
                sample = {
                    'task_type': task_type,
                    'sample_id': int(row['sample_id']),
                    'actions': actions,
                    'expected_result': expected_result,
                    'model_name': row['model_name']
                }
                
                # Add property field for projection and verification tasks
                if task_type in ['projection', 'verification']:
                    sample['property'] = row.get('property', '').strip()
                
                model_name = row['model_name'] # group by model
                if model_name not in samples_by_model:
                    samples_by_model[model_name] = []
                samples_by_model[model_name].append(sample)
        
        return samples_by_model
    
    def evaluate_model(self, model_name: str, samples: List[Dict]):
        """Evaluate a single BPMN model with its samples.
        
        Args:
            model_name: Name of the model (e.g., "process_1.bpmn")
            samples: List of sample dictionaries for this model
        """
        print(f"\n{'='*70}")
        print(f"Evaluating: {model_name}")
        print(f"{'='*70}")
        
        bpmn_file = os.path.join(self.dataset_dir, model_name)
        if not os.path.exists(bpmn_file):
            print(f"  Error: BPMN file not found: {bpmn_file}")
            return
        model_id = self.translate_bpmn_model(bpmn_file, model_name)
        if not model_id:
            print(f"  Error: Translation failed for {model_name}")
            return
        print(f"  Translation successful: {model_id}")
        try:
            # Initialize reasoner with evaluation_temp directory
            reasoner = ReasoningService(model_id, model_base_dir=self.pl_models_dir)
            print(f"  Reasoner initialized")
        except Exception as e:
            print(f"  Error initializing reasoner: {e}")
            return
        
        correct = 0
        total = 0
        for sample in samples:
            total += 1
            task_type = sample['task_type']
            print(f"\n  Sample {sample['sample_id']} ({task_type}):")
            
            if task_type in ['projection', 'verification']:
                print(f"    Property: {sample.get('property', 'N/A')}")
                if sample['actions']:
                    print(f"    Actions: {' -> '.join(sample['actions'][:5])}{'...' if len(sample['actions']) > 5 else ''}")
            else:
                print(f"    Actions: {' -> '.join(sample['actions'][:5])}{'...' if len(sample['actions']) > 5 else ''}")
            
            print(f"    Expected: {sample['expected_result']}")
            
            if task_type == 'legality':
                metrics = self.run_legality_task(reasoner, sample)
            elif task_type == 'conformance':
                metrics = self.run_conformance_task(reasoner, sample)
            elif task_type == 'projection':
                metrics = self.run_projection_task(reasoner, sample)
            elif task_type == 'verification':
                metrics = self.run_verification_task(reasoner, sample)
            else:
                print(f"    Unknown task type: {task_type}")
                continue

            if metrics:
                # Check correctness
                # Map result to boolean: success = True, failure = False
                actual_result = metrics.result == 'success'
                is_correct = actual_result == sample['expected_result']
                
                if is_correct:
                    correct += 1
                    print(f"    Correct (result: {metrics.result})")
                else:
                    print(f"    Incorrect (result: {metrics.result}, expected: {sample['expected_result']})")
                
                # Store result
                self.results.append({
                    'model_name': model_name,
                    'task_type': task_type,
                    'sample_id': sample['sample_id'],
                    'expected': sample['expected_result'],
                    'actual': metrics.result,
                    'correct': is_correct,
                    'reasoning_time': metrics.reasoning_time,
                    'inferences': metrics.inferences
                })
            else:
                print(f"    Task failed (no metrics)")
        
        accuracy = (correct / total * 100) if total > 0 else 0
        print(f"\n  Model Accuracy: {correct}/{total} ({accuracy:.1f}%)")
    
    def run_evaluation(self):
        print("\n" + "="*70)
        print("EVALUATION - LEGALITY & CONFORMANCE")
        print("="*70)
        
        csv_path = os.path.join(self.output_dir, 'datasets', 'samples_leg_conf.csv')
        print(f"\nLoading samples from: {csv_path}")
        samples_by_model = self.load_samples_from_csv(csv_path)
        print(f"Loaded {sum(len(s) for s in samples_by_model.values())} samples for {len(samples_by_model)} models")

        for model_name, samples in sorted(samples_by_model.items()):
            self.evaluate_model(model_name, samples)
        self.save_results('_leg_conf')
        self.print_summary()
    
    def run_projection_verification_evaluation(self):
        print("\n" + "="*70)
        print("EVALUATION - PROJECTION & PROPERTY VERIFICATION")
        print("="*70)
        
        csv_path = os.path.join(self.output_dir, 'datasets', 'samples_proj_verif.csv')
        print(f"\nLoading samples from: {csv_path}")
        samples_by_model = self.load_samples_from_csv(csv_path)
        print(f"Loaded {sum(len(s) for s in samples_by_model.values())} samples for {len(samples_by_model)} models")

        # Use exams-bpmn directory for these samples
        exams_dataset_dir = os.path.join(self.project_root, 'bpmn', 'exams-bpmn')
        
        for model_name, samples in sorted(samples_by_model.items()):
            self.evaluate_model_with_dataset(model_name, samples, exams_dataset_dir)
        
        self.save_results('_proj_verif')
        self.print_summary()
    
    def evaluate_model_with_dataset(self, model_name: str, samples: List[Dict], dataset_dir: str):
        """Evaluate a single BPMN model with its samples from a specific dataset directory.
        
        Args:
            model_name: Name of the model (e.g., "process_0.bpmn")
            samples: List of sample dictionaries for this model
            dataset_dir: Directory where the BPMN file is located
        """
        print(f"\n{'='*70}")
        print(f"Evaluating: {model_name}")
        print(f"{'='*70}")
        
        bpmn_file = os.path.join(dataset_dir, model_name)
        if not os.path.exists(bpmn_file):
            print(f"  Error: BPMN file not found: {bpmn_file}")
            return
        
        model_id = self.translate_bpmn_model(bpmn_file, model_name)
        if not model_id:
            print(f"  Error: Translation failed for {model_name}")
            return
        print(f"  Translation successful: {model_id}")
        
        try:
            # Initialize reasoner with evaluation_temp directory
            reasoner = ReasoningService(model_id, model_base_dir=self.pl_models_dir)
            print(f"  Reasoner initialized")
        except Exception as e:
            print(f"  Error initializing reasoner: {e}")
            return
        
        correct = 0
        total = 0
        for sample in samples:
            total += 1
            task_type = sample['task_type']
            print(f"\n  Sample {sample['sample_id']} ({task_type}):")
            
            if task_type in ['projection', 'verification']:
                print(f"    Property: {sample.get('property', 'N/A')}")
                if sample['actions']:
                    print(f"    Actions: {' -> '.join(sample['actions'][:5])}{'...' if len(sample['actions']) > 5 else ''}")
            else:
                print(f"    Actions: {' -> '.join(sample['actions'][:5])}{'...' if len(sample['actions']) > 5 else ''}")
            
            print(f"    Expected: {sample['expected_result']}")
            
            if task_type == 'legality':
                metrics = self.run_legality_task(reasoner, sample)
            elif task_type == 'conformance':
                metrics = self.run_conformance_task(reasoner, sample)
            elif task_type == 'projection':
                metrics = self.run_projection_task(reasoner, sample)
            elif task_type == 'verification':
                metrics = self.run_verification_task(reasoner, sample)
            else:
                print(f"    Unknown task type: {task_type}")
                continue

            if metrics:
                # Check correctness
                # Map result to boolean: success = True, failure = False
                actual_result = metrics.result == 'success'
                is_correct = actual_result == sample['expected_result']
                
                if is_correct:
                    correct += 1
                    print(f"    Correct (result: {metrics.result})")
                else:
                    print(f"    Incorrect (result: {metrics.result}, expected: {sample['expected_result']})")
                
                # Store result
                self.results.append({
                    'model_name': model_name,
                    'task_type': task_type,
                    'sample_id': sample['sample_id'],
                    'expected': sample['expected_result'],
                    'actual': metrics.result,
                    'correct': is_correct,
                    'reasoning_time': metrics.reasoning_time,
                    'inferences': metrics.inferences
                })
            else:
                print(f"    Task failed (no metrics)")
        
        accuracy = (correct / total * 100) if total > 0 else 0
        print(f"\n  Model Accuracy: {correct}/{total} ({accuracy:.1f}%)")
    
    def save_results(self, suffix=''):
        """Save evaluation results to CSV file.
        
        Args:
            suffix: Optional suffix for the output filename (e.g., '_proj_verif')
        """
        filename = f'evaluation_results{suffix}.csv'
        output_file = os.path.join(self.output_dir, 'results', filename)        
        with open(output_file, 'w', newline='') as f:
            if self.results:
                writer = csv.DictWriter(f, fieldnames=self.results[0].keys())
                writer.writeheader()
                writer.writerows(self.results)
        
        print(f"\n Results saved to: {output_file}")
    
    def print_summary(self):
        if not self.results:
            print("\nNo results to summarize.")
            return
        
        total = len(self.results)
        correct = sum(1 for r in self.results if r['correct'])
        accuracy = (correct / total * 100) if total > 0 else 0
        
        # Group by task type
        legality_results = [r for r in self.results if r['task_type'] == 'legality']
        conformance_results = [r for r in self.results if r['task_type'] == 'conformance']
        projection_results = [r for r in self.results if r['task_type'] == 'projection']
        verification_results = [r for r in self.results if r['task_type'] == 'verification']
        
        legality_correct = sum(1 for r in legality_results if r['correct'])
        conformance_correct = sum(1 for r in conformance_results if r['correct'])
        projection_correct = sum(1 for r in projection_results if r['correct'])
        verification_correct = sum(1 for r in verification_results if r['correct'])
        
        legality_acc = (legality_correct / len(legality_results) * 100) if legality_results else 0
        conformance_acc = (conformance_correct / len(conformance_results) * 100) if conformance_results else 0
        projection_acc = (projection_correct / len(projection_results) * 100) if projection_results else 0
        verification_acc = (verification_correct / len(verification_results) * 100) if verification_results else 0
        
        print("\n" + "="*70)
        print("EVALUATION SUMMARY")
        print("="*70)
        print(f"Total samples: {total}")
        print(f"Correct: {correct}")
        print(f"Overall Accuracy: {accuracy:.1f}%")
        print()
        
        if legality_results:
            print(f"Legality: {legality_correct}/{len(legality_results)} ({legality_acc:.1f}%)")
        if conformance_results:
            print(f"Conformance: {conformance_correct}/{len(conformance_results)} ({conformance_acc:.1f}%)")
        if projection_results:
            print(f"Projection: {projection_correct}/{len(projection_results)} ({projection_acc:.1f}%)")
        if verification_results:
            print(f"Property Verification: {verification_correct}/{len(verification_results)} ({verification_acc:.1f}%)")
        
        print("="*70)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='BPMN Evaluation - Legality, Conformance, Projection, and Property Verification')
    parser.add_argument('--task', type=str, default='all', 
                       choices=['legality_conformance', 'projection_verification', 'all'],
                       help='Which evaluation to run: legality_conformance, projection_verification, or all (default: all)')
    args = parser.parse_args()
    
    project_root = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..'))
    dataset_dir = os.path.join(project_root, 'bpmn', 'dataset', 'processed')
    output_dir = os.path.join(project_root, 'evaluation')
    
    evaluator = BPMNEvaluator(dataset_dir, output_dir)
    
    if args.task in ['legality_conformance', 'all']:
        evaluator.run_evaluation()
        
    if args.task in ['projection_verification', 'all']:
        # Reset results for separate evaluation
        if args.task == 'all':
            evaluator.results = []
        evaluator.run_projection_verification_evaluation()
