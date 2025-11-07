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
            actions_str = ', '.join(sample['actions'])
            parameters = {'actions': actions_str}
            success, output = reasoner.execute_task('legality', parameters)
            metrics = MetricsCollector.parse_prolog_output(output, success)
            return metrics
        
        except Exception as e:
            print(f"    Error in legality task: {e}")
            return None
    
    def run_conformance_task(self, reasoner: ReasoningService, sample: Dict) -> Optional[ReasoningMetrics]:
        try:
            history_str = ', '.join(sample['actions']) # ReasoningService will reverse the history
            parameters = {'history_actions': history_str}
            success, output = reasoner.execute_task('conformance', parameters)
            metrics = MetricsCollector.parse_prolog_output(output, success)
            return metrics
        
        except Exception as e:
            print(f"    Error in conformance task: {e}")
            return None

    
    def load_samples_from_csv(self, csv_path: str) -> Dict[str, List[Dict]]:
        samples_by_model = {}
        with open(csv_path, 'r', newline='') as f:
            reader = csv.DictReader(f)
            for row in reader:
                if row['task_type'] not in ['legality', 'conformance']:
                    continue    
                actions = [a.strip() for a in row['actions'].split(';') if a.strip()]
                expected_result = row['expected_result'].lower() == 'true'
                sample = {
                    'task_type': row['task_type'],
                    'sample_id': int(row['sample_id']),
                    'actions': actions,
                    'expected_result': expected_result,
                    'model_name': row['model_name']
                }
                
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
            print(f"    Actions: {' -> '.join(sample['actions'][:5])}{'...' if len(sample['actions']) > 5 else ''}")
            print(f"    Expected: {sample['expected_result']}")
            if task_type == 'legality':
                metrics = self.run_legality_task(reasoner, sample)
            else:
                metrics = self.run_conformance_task(reasoner, sample)

            if metrics:
                # Check correctness
                # Map result to boolean: success/true/conforms = True, failure/false/not_conforms/unknown = False
                actual_result = metrics.result in ['true', 'success', 'conforms']
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
        self.save_results()
        self.print_summary()
    
    def save_results(self):
        output_file = os.path.join(self.output_dir, 'results', 'evaluation_results.csv')        
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
        
        legality_results = [r for r in self.results if r['task_type'] == 'legality']
        conformance_results = [r for r in self.results if r['task_type'] == 'conformance']
        legality_correct = sum(1 for r in legality_results if r['correct'])
        conformance_correct = sum(1 for r in conformance_results if r['correct'])
        legality_acc = (legality_correct / len(legality_results) * 100) if legality_results else 0
        conformance_acc = (conformance_correct / len(conformance_results) * 100) if conformance_results else 0
        
        print("\n" + "="*70)
        print("EVALUATION SUMMARY")
        print("="*70)
        print(f"Total samples: {total}")
        print(f"Correct: {correct}")
        print(f"Overall Accuracy: {accuracy:.1f}%")
        print(f"\nLegality: {legality_correct}/{len(legality_results)} ({legality_acc:.1f}%)")
        print(f"Conformance: {conformance_correct}/{len(conformance_results)} ({conformance_acc:.1f}%)")
        print("="*70)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Simplified BPMN Evaluation')
    args = parser.parse_args()
    
    project_root = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..'))
    dataset_dir = os.path.join(project_root, 'bpmn', 'dataset', 'processed')
    output_dir = os.path.join(project_root, 'evaluation')
    
    evaluator = BPMNEvaluator(dataset_dir, output_dir)
    evaluator.run_evaluation()
