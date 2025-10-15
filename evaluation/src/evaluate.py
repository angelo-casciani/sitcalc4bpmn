"""
BPMN Evaluation Script

This script performs comprehensive evaluation of BPMN models:
1. Iterates over all BPMN models in dataset/processed
2. Translates each model using bpmn2indi_cli
3. Generates test samples
4. Runs reasoning tasks (projection, legality, conformance, property verification)
5. Collects performance metrics
6. Computes accuracy metrics
7. Outputs results to CSV and TXT

Usage:
    python evaluate.py [--models MODEL1 MODEL2 ...] [--limit N]
"""

import argparse
import os
import sys
import csv
import time
import tracemalloc
from pathlib import Path
from typing import List, Dict, Optional
import subprocess

# Add both evaluation src and main src to path
eval_src_dir = os.path.dirname(os.path.abspath(__file__))
main_src_dir = os.path.abspath(os.path.join(eval_src_dir, '..', '..', 'src'))
sys.path.insert(0, eval_src_dir)
sys.path.insert(0, main_src_dir)

from bpmn_metrics import extract_bpmn_metrics, BPMNMetrics
from sample_generator import SampleGenerator, ReasoningSample, ReasoningTask, ExpectedResult
from metrics_collector import MetricsCollector, ReasoningMetrics, TranslationMetrics
from bpmn_parser import BPMNParser
from prolog_translator import PrologTranslator
from prolog_templates import MAIN_PL_TEMPLATE


class BPMNEvaluator:
    """Main evaluator for BPMN reasoning performance."""
    
    def __init__(self, dataset_dir: str, output_dir: str, regenerate_samples: bool = False):
        """Initialize the evaluator.
        
        Args:
            dataset_dir: Path to processed dataset directory
            output_dir: Path to evaluation output directory
            regenerate_samples: Force regeneration of samples even if they exist
        """
        self.dataset_dir = dataset_dir
        self.output_dir = output_dir
        self.regenerate_samples = regenerate_samples
        self.results = []
        
        # Ensure output directories exist
        os.makedirs(os.path.join(output_dir, 'datasets'), exist_ok=True)
        os.makedirs(os.path.join(output_dir, 'results'), exist_ok=True)
        
        # Project paths (evaluation/src -> project root is ../..)
        self.project_root = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..'))
        self.pl_models_dir = os.path.join(self.project_root, 'pl_models')
    
    def find_all_bpmn_models(self) -> List[tuple]:
        """Find all BPMN models in the processed dataset.
        
        Returns:
            List of tuples (folder_name, model_name, bpmn_file_path)
        """
        models = []
        
        if not os.path.exists(self.dataset_dir):
            print(f"Error: Dataset directory not found: {self.dataset_dir}")
            return models
        
        # Iterate through subdirectories (E_j01, E_j02, etc.)
        for folder in sorted(os.listdir(self.dataset_dir)):
            folder_path = os.path.join(self.dataset_dir, folder)
            
            if not os.path.isdir(folder_path):
                continue
            
            # Find BPMN files in this folder
            for file in os.listdir(folder_path):
                if file.endswith('.bpmn2.xml') or file.endswith('.bpmn') or file.endswith('.xml'):
                    if not file.endswith('.txt'):  # Skip text files
                        bpmn_file_path = os.path.join(folder_path, file)
                        model_name = f"{folder}_{file.replace('.bpmn2.xml', '').replace('.bpmn', '').replace('.xml', '')}"
                        models.append((folder, model_name, bpmn_file_path))
        
        return models
    
    def translate_bpmn_model(self, bpmn_file_path: str, model_name: str) -> Optional[TranslationMetrics]:
        """Translate a BPMN model to IndiGolog using the src translation functions.
        
        Args:
            bpmn_file_path: Path to BPMN file
            model_name: Name for the model
            
        Returns:
            TranslationMetrics or None if translation failed
        """
        try:
            # Create output directory
            model_dir = os.path.join(self.pl_models_dir, model_name)
            os.makedirs(model_dir, exist_ok=True)
            
            output_pl_path = os.path.join(model_dir, f'{model_name}.pl')
            main_pl_path = os.path.join(model_dir, 'main.pl')
            
            # Start memory and time tracking
            tracemalloc.start()
            start_time = time.time()
            
            try:
                # Use the EXACT same translation logic as src/bpmn2indi_cli.py
                # Step 1: Parse BPMN
                parser = BPMNParser(bpmn_file_path)
                
                # Step 2: Translate to Prolog
                translator = PrologTranslator(parser)
                prolog_code = translator.translate()
                
                # Step 3: Write the translated .pl file
                with open(output_pl_path, 'w') as f:
                    f.write(prolog_code)
                
                # Step 4: Generate main.pl using the EXACT template from src
                main_pl_content = MAIN_PL_TEMPLATE.format(prolog_basename=model_name)
                with open(main_pl_path, 'w') as f:
                    f.write(main_pl_content)
                
                translation_time = time.time() - start_time
                current, peak = tracemalloc.get_traced_memory()
                memory_usage_mb = peak / (1024 * 1024)
                
            finally:
                tracemalloc.stop()
            
            # Analyze generated program
            num_actions, num_fluents, program_lines = MetricsCollector._analyze_prolog_program(
                output_pl_path
            )
            
            return TranslationMetrics(
                translation_time=translation_time,
                memory_usage=memory_usage_mb,
                num_actions=num_actions,
                num_fluents=num_fluents,
                program_size_lines=program_lines
            )
        
        except Exception as e:
            print(f"  Error translating {model_name}: {e}")
            return None
    
    def run_projection_task(self, reasoner, sample: ReasoningSample) -> Optional[ReasoningMetrics]:
        """Run a projection reasoning task.
        
        Args:
            reasoner: IndiGologReasoner instance to use
            sample: Projection sample
            
        Returns:
            ReasoningMetrics or None if task failed
        """
        try:
            expected = sample.expected_result == ExpectedResult.TRUE
            expected_str = 'true' if expected else 'false'
            
            print(f"    [DEBUG] Calling projection with:")
            print(f"            Fluent: {sample.fluent}")
            print(f"            Actions: {sample.actions}")
            print(f"            Expected: {expected_str}")
            
            success, output = reasoner.projection(sample.fluent, sample.actions, expected_str)
            
            print(f"    [DEBUG] Projection returned:")
            print(f"            Success: {success}")
            print(f"            Output length: {len(output)} chars")
            
            metrics = MetricsCollector.parse_prolog_output(output, success)
            
            # For projection: convert success/failure to true/false based on expected value
            # If query succeeds -> fluent has expected value
            # If query fails -> fluent has opposite value
            if metrics:
                if metrics.result == "success":
                    metrics.result = expected_str  # Query succeeded, fluent has expected value
                elif metrics.result == "failure":
                    metrics.result = 'false' if expected else 'true'  # Query failed, fluent has opposite value
            
            print(f"    [DEBUG] Parsed metrics:")
            print(f"            Result: {metrics.result if metrics else 'None'}")
            print(f"            Time: {metrics.reasoning_time if metrics else 'N/A'}")
            print(f"            Inferences: {metrics.inferences if metrics else 'N/A'}")
            
            return metrics
        
        except Exception as e:
            print(f"    [ERROR] Exception in projection task: {e}")
            import traceback
            traceback.print_exc()
            return None
    
    def run_legality_task(self, reasoner, sample: ReasoningSample) -> Optional[ReasoningMetrics]:
        """Run a legality reasoning task.
        
        Args:
            reasoner: IndiGologReasoner instance to use
            sample: Legality sample
            
        Returns:
            ReasoningMetrics or None if task failed
        """
        try:
            proc_name = f"test_legality_{sample.sample_id}"
            
            print(f"    [DEBUG] Calling legality with:")
            print(f"            Procedure: {proc_name}")
            print(f"            Actions: {sample.actions}")
            print(f"            Expected: {sample.expected_result.value}")
            
            success, output = reasoner.legality(proc_name, sample.actions)
            
            print(f"    [DEBUG] Legality returned:")
            print(f"            Success flag: {success}")
            print(f"            Output length: {len(output)} chars")
            
            # Show key parts of output for debugging
            if "RESULT: SUCCESS" in output:
                print(f"            Output contains: 'RESULT: SUCCESS'")
            if "RESULT: FAILURE" in output:
                print(f"            Output contains: 'RESULT: FAILURE'")
            if "Program has executed to completion" in output:
                print(f"            Output contains: 'Program has executed to completion'")
            if "Program fails" in output:
                print(f"            Output contains: 'Program fails'")
            
            metrics = MetricsCollector.parse_prolog_output(output, success)
            
            print(f"    [DEBUG] Parsed metrics:")
            print(f"            Result: {metrics.result if metrics else 'None'}")
            print(f"            Success from reason.py: {success}")
            print(f"            Time: {metrics.reasoning_time if metrics else 'N/A'}")
            print(f"            Inferences: {metrics.inferences if metrics else 'N/A'}")
            
            return metrics
        
        except Exception as e:
            print(f"    [ERROR] Exception in legality task: {e}")
            import traceback
            traceback.print_exc()
            return None
    
    def run_conformance_task(self, reasoner, sample: ReasoningSample) -> Optional[ReasoningMetrics]:
        """Run a conformance checking task.
        
        Args:
            reasoner: IndiGologReasoner instance to use
            sample: Conformance sample
            
        Returns:
            ReasoningMetrics or None if task failed
        """
        try:
            print(f"    [DEBUG] Calling conformance_checking with:")
            print(f"            History length: {len(sample.actions)}")
            print(f"            Actions: {sample.actions}")
            
            success, output = reasoner.conformance_checking(sample.actions)
            
            print(f"    [DEBUG] Conformance returned:")
            print(f"            Success: {success}")
            print(f"            Output length: {len(output)} chars")
            print(f"            Output contains 'RESULT: CONFORMANT': {'RESULT: CONFORMANT' in output}")
            print(f"            Output contains 'RESULT: NON-CONFORMANT': {'RESULT: NON-CONFORMANT' in output}")
            
            metrics = MetricsCollector.parse_prolog_output(output, success)
            
            print(f"    [DEBUG] Parsed metrics:")
            print(f"            Result: {metrics.result if metrics else 'None'}")
            print(f"            Time: {metrics.reasoning_time if metrics else 'N/A'}")
            print(f"            Inferences: {metrics.inferences if metrics else 'N/A'}")
            
            return metrics
        
        except Exception as e:
            print(f"    [ERROR] Exception in conformance task: {e}")
            import traceback
            traceback.print_exc()
            return None
    
    def run_property_verification_task(self, reasoner, sample: ReasoningSample) -> Optional[ReasoningMetrics]:
        """Run a property verification task.
        
        Args:
            reasoner: IndiGologReasoner instance to use
            sample: Property verification sample
            
        Returns:
            ReasoningMetrics or None if task failed
        """
        try:
            proc_name = f"verify_property_{sample.sample_id}"
            
            print(f"    [DEBUG] Calling verify_property with:")
            print(f"            Procedure: {proc_name}")
            print(f"            Property: {sample.property_expr}")
            
            success, output = reasoner.verify_property(sample.property_expr, proc_name)
            
            print(f"    [DEBUG] Property verification returned:")
            print(f"            Success: {success}")
            print(f"            Output length: {len(output)} chars")
            print(f"            Output contains 'RESULT: SUCCESS': {'RESULT: SUCCESS' in output}")
            print(f"            Output contains 'RESULT: FAILURE': {'RESULT: FAILURE' in output}")
            print(f"            Output contains 'ERROR': {'ERROR' in output}")
            
            metrics = MetricsCollector.parse_prolog_output(output, success)
            
            print(f"    [DEBUG] Parsed metrics:")
            print(f"            Result: {metrics.result if metrics else 'None'}")
            print(f"            Time: {metrics.reasoning_time if metrics else 'N/A'}")
            print(f"            Inferences: {metrics.inferences if metrics else 'N/A'}")
            
            return metrics
        
        except Exception as e:
            print(f"    [ERROR] Exception in property verification task: {e}")
            import traceback
            traceback.print_exc()
            return None
    
    def evaluate_model(self, folder_name: str, model_name: str, bpmn_file_path: str) -> Dict:
        """Evaluate a single BPMN model.
        
        Args:
            folder_name: Folder containing the model
            model_name: Name of the model
            bpmn_file_path: Path to BPMN file
            
        Returns:
            Dictionary with evaluation results
        """
        print(f"\n{'='*70}")
        print(f"Evaluating: {model_name}")
        print(f"File: {bpmn_file_path}")
        print(f"{'='*70}")
        
        result = {
            'folder': folder_name,
            'model_name': model_name,
            'bpmn_file': bpmn_file_path
        }
        
        # Step 1: Extract BPMN metrics
        print("\n[1/5] Extracting BPMN metrics...")
        try:
            bpmn_metrics = extract_bpmn_metrics(bpmn_file_path)
            result.update(bpmn_metrics.to_dict())
            print(f"  Tasks: {bpmn_metrics.num_tasks}, Gateways: {bpmn_metrics.num_exclusive_gateways}+{bpmn_metrics.num_parallel_gateways}")
        except Exception as e:
            print(f"  Error extracting metrics: {e}")
            return result
        
        # Step 2: Translate model
        print("\n[2/5] Translating BPMN to IndiGolog...")
        translation_metrics = self.translate_bpmn_model(bpmn_file_path, model_name)
        
        if translation_metrics:
            result.update(translation_metrics.to_dict())
            print(f"  Translation time: {translation_metrics.translation_time:.3f}s")
            print(f"  Memory: {translation_metrics.memory_usage:.2f} MB")
            print(f"  Actions: {translation_metrics.num_actions}, Fluents: {translation_metrics.num_fluents}")
        else:
            print("  Translation failed, skipping model")
            return result
        
        # Step 3: Generate or load samples
        samples_csv_path = os.path.join(self.output_dir, 'datasets', f'{model_name}_samples.csv')
        
        if os.path.exists(samples_csv_path) and not self.regenerate_samples:
            print("\n[3/5] Loading existing test samples...")
            samples = self._load_samples_from_csv(samples_csv_path)
            print(f"  Loaded {len(samples)} samples from existing file")
        else:
            if self.regenerate_samples and os.path.exists(samples_csv_path):
                print("\n[3/5] Regenerating test samples (--regenerate-samples flag set)...")
            else:
                print("\n[3/5] Generating test samples...")
            
            generator = SampleGenerator(model_name, bpmn_metrics)
            samples = generator.generate_all_samples()
            print(f"  Generated {len(samples)} samples")
            
            # Save samples to CSV for future reuse
            self._save_samples_to_csv(samples, samples_csv_path)
            print(f"  Saved samples to {samples_csv_path}")
        
        # Step 4: Run reasoning tasks
        print("\n[4/5] Running reasoning tasks...")
        task_results = []
        
        # Create a single reasoner instance for all tasks on this model
        from reason import IndiGologReasoner
        print(f"  Creating single IndiGologReasoner for model: {model_name}")
        reasoner = IndiGologReasoner(model_name)
        
        for i, sample in enumerate(samples, 1):
            print(f"  [{i}/{len(samples)}] {sample.task_type.value} - Sample {sample.sample_id}")
            
            metrics = None
            
            if sample.task_type == ReasoningTask.PROJECTION:
                metrics = self.run_projection_task(reasoner, sample)
            elif sample.task_type == ReasoningTask.LEGALITY:
                metrics = self.run_legality_task(reasoner, sample)
            elif sample.task_type == ReasoningTask.CONFORMANCE:
                metrics = self.run_conformance_task(reasoner, sample)
            elif sample.task_type == ReasoningTask.PROPERTY_VERIFICATION:
                metrics = self.run_property_verification_task(reasoner, sample)
            
            if metrics:
                # Compute correctness: does actual match expected?
                correct = (sample.expected_result.value == metrics.result)
                
                task_result = {
                    'task_type': sample.task_type.value,
                    'sample_id': sample.sample_id,
                    'expected': sample.expected_result.value,
                    'actual': metrics.result,
                    'correct': correct,
                    'reasoning_time_sec': metrics.reasoning_time,
                    'cpu_time_sec': metrics.cpu_time,
                    'inferences': metrics.inferences
                }
                task_results.append(task_result)
                print(f"    Expected: {sample.expected_result.value}, Actual: {metrics.result}, Correct: {correct}")
                print(f"    Time: {metrics.reasoning_time:.3f}s, Inferences: {metrics.inferences}")
        
        # Step 5: Compute accuracy metrics
        print("\n[5/5] Computing accuracy metrics...")
        accuracy_by_task = {}
        
        for task_type in ReasoningTask:
            task_samples = [r for r in task_results if r['task_type'] == task_type.value]
            
            if task_samples:
                predictions = [self._result_to_bool(r['actual']) for r in task_samples]
                ground_truth = [self._result_to_bool(r['expected']) for r in task_samples]
                
                accuracy_metrics = MetricsCollector.compute_accuracy_metrics(predictions, ground_truth)
                accuracy_by_task[task_type.value] = accuracy_metrics
                
                print(f"  {task_type.value}: Accuracy={accuracy_metrics['accuracy']:.3f}, F1={accuracy_metrics['f1_score']:.3f}")
        
        result['task_results'] = task_results
        result['accuracy_by_task'] = accuracy_by_task
        
        return result
    
    def _result_to_bool(self, result_str: str) -> bool:
        """Convert result string to boolean.
        
        Args:
            result_str: Result string (success/failure/true/false/conforms/not_conforms)
            
        Returns:
            Boolean value
        """
        positive = ['success', 'true', 'conforms']
        return result_str.lower() in positive
    
    def _save_samples_to_csv(self, samples: List[ReasoningSample], output_path: str):
        """Save generated samples to CSV file.
        
        Args:
            samples: List of samples
            output_path: Path to output CSV file
        """
        if not samples:
            return
        
        with open(output_path, 'w', newline='') as f:
            writer = csv.DictWriter(f, fieldnames=samples[0].to_dict().keys())
            writer.writeheader()
            for sample in samples:
                writer.writerow(sample.to_dict())
    
    def _parse_action_list(self, action_string: str) -> List[str]:
        """Parse a semicolon-separated action list.
        
        Actions are now separated by semicolons to avoid CSV parsing issues
        with commas inside action parameters, e.g.:
        "action1(a,b);action2;action3(x,y,z)"
        
        Args:
            action_string: String containing semicolon-separated actions
        
        Returns:
            List of action strings
        """
        if not action_string or not action_string.strip():
            return []
        
        # Simply split on semicolon since actions no longer contain this character
        actions = [action.strip() for action in action_string.split(';') if action.strip()]
        
        return actions
    
    def _load_samples_from_csv(self, input_path: str) -> List[ReasoningSample]:
        """Load samples from CSV file.
        
        Args:
            input_path: Path to CSV file with samples
            
        Returns:
            List of ReasoningSample objects
        """
        samples = []
        
        with open(input_path, 'r', newline='') as f:
            reader = csv.DictReader(f)
            for row in reader:
                # Parse actions respecting parentheses
                actions = self._parse_action_list(row['actions'])
                
                # Parse task type
                task_type = ReasoningTask(row['task_type'])
                
                # Parse expected result
                expected_result = ExpectedResult(row['expected_result'])
                
                # Create ReasoningSample object
                sample = ReasoningSample(
                    task_type=task_type,
                    sample_id=int(row['sample_id']),
                    actions=actions,
                    fluent=row['fluent'] if row['fluent'] else None,
                    property_expr=row['property_expr'] if row['property_expr'] else None,
                    expected_result=expected_result,
                    description=row['description']
                )
                samples.append(sample)
        
        return samples
    
    def run_evaluation(self, model_filter: Optional[List[str]] = None, limit: Optional[int] = None):
        """Run evaluation on all or filtered models.
        
        Args:
            model_filter: List of specific model names to evaluate (None = all)
            limit: Maximum number of models to evaluate (None = no limit)
        """
        print("\n" + "="*70)
        print("BPMN REASONING EVALUATION")
        print("="*70)
        
        # Find all models
        all_models = self.find_all_bpmn_models()
        print(f"\nFound {len(all_models)} BPMN models in dataset")
        
        # Apply filters
        if model_filter:
            all_models = [(f, m, p) for f, m, p in all_models if any(filt in m for filt in model_filter)]
            print(f"Filtered to {len(all_models)} models matching: {model_filter}")
        
        if limit:
            all_models = all_models[:limit]
            print(f"Limited to first {limit} models")
        
        # Evaluate each model
        for i, (folder, model_name, bpmn_file) in enumerate(all_models, 1):
            print(f"\n\nModel {i}/{len(all_models)}")
            
            result = self.evaluate_model(folder, model_name, bpmn_file)
            self.results.append(result)
        
        # Generate reports
        print(f"\n\n{'='*70}")
        print("GENERATING REPORTS")
        print("="*70)
        
        self.save_results()
        self.generate_summary_report()
    
    def save_results(self):
        """Save detailed results to CSV."""
        results_csv_path = os.path.join(self.output_dir, 'results', 'detailed_results.csv')
        
        print(f"\nSaving detailed results to: {results_csv_path}")
        
        # Flatten results for CSV
        rows = []
        for result in self.results:
            base_row = {
                'folder': result.get('folder', ''),
                'model_name': result.get('model_name', ''),
                'num_tasks': result.get('num_tasks', 0),
                'num_exclusive_gateways': result.get('num_exclusive_gateways', 0),
                'num_parallel_gateways': result.get('num_parallel_gateways', 0),
                'num_events': result.get('num_events', 0),
                'translation_time_sec': result.get('translation_time_sec', 0),
                'memory_usage_mb': result.get('memory_usage_mb', 0),
                'num_actions': result.get('num_actions', 0),
                'num_fluents': result.get('num_fluents', 0),
            }
            
            # Add task-specific results
            for task_result in result.get('task_results', []):
                row = base_row.copy()
                row.update(task_result)
                rows.append(row)
        
        if rows:
            with open(results_csv_path, 'w', newline='') as f:
                writer = csv.DictWriter(f, fieldnames=rows[0].keys())
                writer.writeheader()
                writer.writerows(rows)
    
    def generate_summary_report(self):
        """Generate summary report in TXT format."""
        report_path = os.path.join(self.output_dir, 'results', 'summary_report.txt')
        
        print(f"Generating summary report: {report_path}")
        
        with open(report_path, 'w') as f:
            f.write("="*70 + "\n")
            f.write("BPMN REASONING EVALUATION - SUMMARY REPORT\n")
            f.write("="*70 + "\n\n")
            
            f.write(f"Total models evaluated: {len(self.results)}\n\n")
            
            # Per-model summary
            for result in self.results:
                f.write(f"\n{'-'*70}\n")
                f.write(f"Model: {result.get('model_name', 'Unknown')}\n")
                f.write(f"{'-'*70}\n")
                f.write(f"BPMN Metrics:\n")
                f.write(f"  Tasks: {result.get('num_tasks', 0)}\n")
                f.write(f"  Exclusive Gateways: {result.get('num_exclusive_gateways', 0)}\n")
                f.write(f"  Parallel Gateways: {result.get('num_parallel_gateways', 0)}\n")
                f.write(f"  Events: {result.get('num_events', 0)}\n")
                f.write(f"\nTranslation Metrics:\n")
                f.write(f"  Time: {result.get('translation_time_sec', 0):.3f} sec\n")
                f.write(f"  Memory: {result.get('memory_usage_mb', 0):.2f} MB\n")
                f.write(f"  Actions: {result.get('num_actions', 0)}\n")
                f.write(f"  Fluents: {result.get('num_fluents', 0)}\n")
                
                f.write(f"\nAccuracy by Task Type:\n")
                for task_type, metrics in result.get('accuracy_by_task', {}).items():
                    f.write(f"  {task_type}:\n")
                    f.write(f"    Accuracy:  {metrics['accuracy']:.3f}\n")
                    f.write(f"    Precision: {metrics['precision']:.3f}\n")
                    f.write(f"    Recall:    {metrics['recall']:.3f}\n")
                    f.write(f"    F1 Score:  {metrics['f1_score']:.3f}\n")
                    f.write(f"    MCC:       {metrics['mcc']:.3f}\n")
            
            # Overall averages
            f.write(f"\n{'='*70}\n")
            f.write("OVERALL AVERAGES\n")
            f.write(f"{'='*70}\n")
            
            if self.results:
                avg_translation_time = sum(r.get('translation_time_sec', 0) for r in self.results) / len(self.results)
                avg_memory = sum(r.get('memory_usage_mb', 0) for r in self.results) / len(self.results)
                avg_actions = sum(r.get('num_actions', 0) for r in self.results) / len(self.results)
                
                f.write(f"Average Translation Time: {avg_translation_time:.3f} sec\n")
                f.write(f"Average Memory Usage: {avg_memory:.2f} MB\n")
                f.write(f"Average # Actions: {avg_actions:.1f}\n")


def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description="Evaluate BPMN reasoning performance on processed dataset"
    )
    parser.add_argument(
        '--dataset-dir',
        default='../../bpmn/dataset/processed',
        help='Path to processed BPMN dataset directory'
    )
    parser.add_argument(
        '--output-dir',
        default='..',
        help='Path to evaluation output directory'
    )
    parser.add_argument(
        '--models',
        nargs='+',
        help='Specific models to evaluate (filter by name)'
    )
    parser.add_argument(
        '--limit',
        type=int,
        help='Maximum number of models to evaluate'
    )
    parser.add_argument(
        '--regenerate-samples',
        action='store_true',
        help='Force regeneration of samples even if they already exist'
    )
    
    args = parser.parse_args()
    
    # Resolve paths relative to script location
    script_dir = os.path.dirname(os.path.abspath(__file__))
    dataset_dir = os.path.abspath(os.path.join(script_dir, args.dataset_dir))
    output_dir = os.path.abspath(os.path.join(script_dir, args.output_dir))
    
    evaluator = BPMNEvaluator(dataset_dir, output_dir, regenerate_samples=args.regenerate_samples)
    evaluator.run_evaluation(model_filter=args.models, limit=args.limit)
    
    print(f"\n{'='*70}")
    print("EVALUATION COMPLETE")
    print(f"{'='*70}")
    print(f"Results saved to: {output_dir}")


if __name__ == '__main__':
    main()
