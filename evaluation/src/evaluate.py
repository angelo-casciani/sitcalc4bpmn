import argparse
import os
import sys
import csv
from typing import List, Dict, Optional

# Add both evaluation src and main src to path
eval_src_dir = os.path.dirname(os.path.abspath(__file__))
main_src_dir = os.path.abspath(os.path.join(eval_src_dir, '..', '..', 'src'))
sys.path.insert(0, eval_src_dir)
sys.path.insert(0, main_src_dir)

from bpmn_metrics import extract_bpmn_metrics
from metrics_collector import MetricsCollector, ReasoningMetrics
from translator_service import TranslatorService
from reasoning_service import ReasoningService


class BPMNEvaluator:
    """Evaluator for BPMN legality and conformance."""
    
    def __init__(self, dataset_dir: str, output_dir: str):
        """Initialize the evaluator.
        
        Args:
            dataset_dir: Path to processed dataset directory
            output_dir: Path to evaluation output directory
        """
        self.dataset_dir = dataset_dir
        self.output_dir = output_dir
        self.results = []
        
        # Ensure output directories exist
        os.makedirs(os.path.join(output_dir, 'results'), exist_ok=True)
        
        # Initialize translator service (like the UI does)
        self.translator = TranslatorService()
        
        # Project paths
        self.project_root = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..'))
        self.pl_models_dir = os.path.join(self.project_root, 'pl_models', 'evaluation_temp')
        os.makedirs(self.pl_models_dir, exist_ok=True)
    
    def translate_bpmn_model(self, bpmn_path: str, model_name: str) -> str:
        """Translate a BPMN model to Prolog using TranslatorService.
        
        Args:
            bpmn_path: Path to the BPMN file
            model_name: Model name (e.g., "process_1.bpmn")
            
        Returns:
            Model name for ReasoningService or None if failed
        """
        try:
            # Generate model name from filename (e.g., "process_1" from "process_1.bpmn")
            model_id = model_name.replace('.bpmn', '')
            eval_model_name = f'{model_id}'
            
            # Use TranslatorService just like the UI does
            success, message, pl_path, prolog_code = self.translator.translate_bpmn_file(bpmn_path, eval_model_name)
            
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
        """Run a legality reasoning task.
        
        Args:
            reasoner: ReasoningService instance
            sample: Sample dictionary with IndiGolog-formatted actions
            
        Returns:
            ReasoningMetrics or None if failed
        """
        try:
            # Actions are already in IndiGolog format from CSV, just join with commas
            actions_str = ', '.join(sample['actions'])
            
            # Use ReasoningService like the UI does
            parameters = {'actions': actions_str}
            success, output = reasoner.execute_task('legality', parameters)
            
            # Parse metrics from output
            metrics = MetricsCollector.parse_prolog_output(output, success)
            return metrics
        except Exception as e:
            print(f"    Error in legality task: {e}")
            return None
    
    def run_conformance_task(self, reasoner: ReasoningService, sample: Dict) -> Optional[ReasoningMetrics]:
        """Run a conformance checking task.
        
        Args:
            reasoner: ReasoningService instance
            sample: Sample dictionary with IndiGolog-formatted actions
            
        Returns:
            ReasoningMetrics or None if failed
        """
        try:
            # Actions are already in IndiGolog format from CSV, just join with commas
            # NOTE: ReasoningService will reverse the history internally
            history_str = ', '.join(sample['actions'])
            
            print(f"    [DEBUG CONFORMANCE] Input (first 3 actions): {sample['actions'][:3]}")
            print(f"    [DEBUG CONFORMANCE] Expected result: {sample['expected_result']}")
            
            # Use ReasoningService like the UI does
            parameters = {'history_actions': history_str}
            success, output = reasoner.execute_task('conformance', parameters)
            
            # Parse metrics from output
            metrics = MetricsCollector.parse_prolog_output(output, success)
            return metrics
        except Exception as e:
            print(f"    Error in conformance task: {e}")
            return None

    
    def load_samples_from_csv(self, csv_path: str) -> Dict[str, List[Dict]]:
        """Load samples from CSV file grouped by model.
        
        CSV now contains IndiGolog-formatted actions (e.g., "start_event(1);activity(start,1);activity(end,1)").
        
        Args:
            csv_path: Path to all_samples.csv
            
        Returns:
            Dictionary mapping model_name to list of samples
        """
        samples_by_model = {}
        
        with open(csv_path, 'r', newline='') as f:
            reader = csv.DictReader(f)
            for row in reader:
                # Only load legality and conformance samples
                if row['task_type'] not in ['legality', 'conformance']:
                    continue
                    
                # Parse actions - already in IndiGolog format from CSV
                actions = [a.strip() for a in row['actions'].split(';') if a.strip()]
                
                # Parse expected result
                expected_result = row['expected_result'].lower() == 'true'
                
                # Create sample
                sample = {
                    'task_type': row['task_type'],
                    'sample_id': int(row['sample_id']),
                    'actions': actions,
                    'expected_result': expected_result,
                    'model_name': row['model_name']
                }
                
                # Group by model
                model_name = row['model_name']
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
        
        # Find BPMN file directly in dataset_dir (no subdirectories)
        bpmn_file = os.path.join(self.dataset_dir, model_name)
        if not os.path.exists(bpmn_file):
            print(f"  Error: BPMN file not found: {bpmn_file}")
            return
        
        # Translate model
        model_id = self.translate_bpmn_model(bpmn_file, model_name)
        if not model_id:
            print(f"  Error: Translation failed for {model_name}")
            return
        
        print(f"  Translation successful: {model_id}")
        
        # Initialize reasoner using ReasoningService (like the UI does)
        try:
            reasoner = ReasoningService(model_id)
            print(f"  Reasoner initialized")
        except Exception as e:
            print(f"  Error initializing reasoner: {e}")
            return
        
        # Run samples
        correct = 0
        total = 0
        
        for sample in samples:
            total += 1
            task_type = sample['task_type']
            
            print(f"\n  Sample {sample['sample_id']} ({task_type}):")
            print(f"    Actions: {' -> '.join(sample['actions'][:5])}{'...' if len(sample['actions']) > 5 else ''}")
            print(f"    Expected: {sample['expected_result']}")
            
            # Run task
            if task_type == 'legality':
                metrics = self.run_legality_task(reasoner, sample)
            else:  # conformance
                metrics = self.run_conformance_task(reasoner, sample)
            
            if metrics:
                # Check correctness
                # Map result to boolean: success/true/conforms = True, failure/false/not_conforms/unknown = False
                actual_result = metrics.result in ['true', 'success', 'conforms']
                is_correct = actual_result == sample['expected_result']
                
                if is_correct:
                    correct += 1
                    print(f"    ✓ Correct (result: {metrics.result})")
                else:
                    print(f"    ✗ Incorrect (result: {metrics.result}, expected: {sample['expected_result']})")
                
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
                print(f"    ✗ Task failed (no metrics)")
        
        accuracy = (correct / total * 100) if total > 0 else 0
        print(f"\n  Model Accuracy: {correct}/{total} ({accuracy:.1f}%)")
    
    def run_evaluation(self):
        """Run complete evaluation."""
        print("\n" + "="*70)
        print("SIMPLIFIED BPMN EVALUATION - LEGALITY & CONFORMANCE")
        print("="*70)
        
        # Load samples
        csv_path = os.path.join(self.output_dir, 'datasets', 'all_samples.csv')
        print(f"\nLoading samples from: {csv_path}")
        
        samples_by_model = self.load_samples_from_csv(csv_path)
        print(f"Loaded {sum(len(s) for s in samples_by_model.values())} samples for {len(samples_by_model)} models")
        
        # Evaluate each model
        for model_name, samples in sorted(samples_by_model.items()):
            self.evaluate_model(model_name, samples)
        
        # Save results
        self.save_results()
        
        # Print summary
        self.print_summary()
    
    def save_results(self):
        """Save results to CSV."""
        output_file = os.path.join(self.output_dir, 'results', 'evaluation_results.csv')
        
        with open(output_file, 'w', newline='') as f:
            if self.results:
                writer = csv.DictWriter(f, fieldnames=self.results[0].keys())
                writer.writeheader()
                writer.writerows(self.results)
        
        print(f"\n✓ Results saved to: {output_file}")
    
    def print_summary(self):
        """Print evaluation summary."""
        if not self.results:
            print("\nNo results to summarize.")
            return
        
        total = len(self.results)
        correct = sum(1 for r in self.results if r['correct'])
        accuracy = (correct / total * 100) if total > 0 else 0
        
        # By task type
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
    
    # Paths
    project_root = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..'))
    dataset_dir = os.path.join(project_root, 'bpmn', 'dataset', 'processed')
    output_dir = os.path.join(project_root, 'evaluation')
    
    # Run evaluation
    evaluator = BPMNEvaluator(dataset_dir, output_dir)
    evaluator.run_evaluation()
