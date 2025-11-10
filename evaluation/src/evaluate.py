import argparse
import datetime
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
from bpmn_metrics import extract_bpmn_metrics


class BPMNEvaluator:
    def __init__(self, dataset_dir: str, output_dir: str, resume_csv: str = None):
        self.dataset_dir = dataset_dir
        self.output_dir = output_dir
        self.results = []
        self.bpmn_metrics = []  # Store BPMN model metrics
        self.translation_metrics = []  # Store translation metrics
        self.current_datetime_str = None  # Store timestamp for file naming
        self.resume_csv = resume_csv  # Path to CSV file for resuming
        self.evaluated_samples = set()  # Track already evaluated sample IDs
        self.current_results_file = None  # Current CSV file path for incremental saving
        self.csv_fieldnames = ['model_name', 'task_type', 'sample_id', 'expected', 'returned', 'correct', 'reasoning_time', 'inferences']
        os.makedirs(os.path.join(output_dir, 'results'), exist_ok=True)
        self.project_root = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..'))
        self.pl_models_dir = os.path.join(self.project_root, 'pl_models', 'evaluation_temp')
        os.makedirs(self.pl_models_dir, exist_ok=True)
        self.translator = TranslatorService(output_dir=self.pl_models_dir)
        
        # Load existing results if resuming
        if self.resume_csv and os.path.exists(self.resume_csv):
            self._load_existing_results()
    
    def _load_existing_results(self):
        """Load existing results from resume CSV file."""
        print(f"\nResuming from: {self.resume_csv}")
        
        # Extract timestamp from filename
        import re
        basename = os.path.basename(self.resume_csv)
        match = re.search(r'(\d{4}-\d{2}-\d{2}_\d{2}:\d{2}:\d{2})', basename)
        if match:
            self.current_datetime_str = match.group(1)
            print(f"Using timestamp: {self.current_datetime_str}")
        
        # Set the current results file to the resume file
        self.current_results_file = self.resume_csv
        
        # Load existing results
        with open(self.resume_csv, 'r', newline='') as f:
            reader = csv.DictReader(f)
            for row in reader:
                # Store the result
                self.results.append({
                    'model_name': row['model_name'],
                    'task_type': row['task_type'],
                    'sample_id': int(row['sample_id']),
                    'expected': row['expected'].lower() == 'true',
                    'returned': row['returned'].lower() == 'true',
                    'correct': row['correct'].lower() == 'true',
                    'reasoning_time': float(row['reasoning_time']),
                    'inferences': int(row['inferences'])
                })
                
                # Track sample ID and model combination
                sample_key = (row['model_name'], int(row['sample_id']))
                self.evaluated_samples.add(sample_key)
        
        print(f"Loaded {len(self.results)} existing results")
        print(f"Will skip {len(self.evaluated_samples)} already evaluated samples\n")
    
    def _initialize_results_file(self, suffix=''):
        """Initialize the results CSV file with headers."""
        if not self.current_results_file:
            # Create new file with timestamp
            self.current_datetime_str = datetime.datetime.now().strftime('%Y-%m-%d_%H:%M:%S')
            filename = f'evaluation_results{suffix}_{self.current_datetime_str}.csv'
            self.current_results_file = os.path.join(self.output_dir, 'results', filename)
            
            # Write header
            with open(self.current_results_file, 'w', newline='') as f:
                writer = csv.DictWriter(f, fieldnames=self.csv_fieldnames)
                writer.writeheader()
            
            print(f"Initialized results file: {self.current_results_file}")
    
    def _append_result_to_file(self, result: Dict):
        """Append a single result to the CSV file immediately."""
        if not self.current_results_file:
            raise RuntimeError("Results file not initialized. Call _initialize_results_file() first.")
        
        # Append the result to the CSV file
        with open(self.current_results_file, 'a', newline='') as f:
            writer = csv.DictWriter(f, fieldnames=self.csv_fieldnames)
            writer.writerow(result)
    
    def translate_bpmn_model(self, bpmn_path: str, model_name: str) -> str:
        try:
            import time
            import re
            
            model_id = model_name.replace('.bpmn', '')
            eval_model_name = f'{model_id}'
            
            # Measure translation time
            start_time = time.time()
            success, message, translated_path, prolog_code = self.translator.translate_bpmn_file(bpmn_path, eval_model_name)
            translation_time = time.time() - start_time
            
            if not success:
                print(f"  Translation failed: {message}")
                return None
            
            # Collect translation metrics
            if prolog_code:
                # Count lines
                program_size_lines = len(prolog_code.split('\n'))
                
                # Count actions (primitiveAction declarations)
                num_actions = len(re.findall(r'primitiveAction\s*\(', prolog_code))
                
                # Count fluents (primitive_fluent declarations)
                num_fluents = len(re.findall(r'primitive_fluent\s*\(', prolog_code))
                
                # Store translation metrics
                self.translation_metrics.append({
                    'model_name': model_name,
                    'translation_time_sec': translation_time,
                    'num_actions': num_actions,
                    'num_fluents': num_fluents,
                    'program_size_lines': program_size_lines
                })
            
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
            # Note: actions are already reversed in the sample, so no need to reverse again
            # Call reasoner method directly to get raw output
            success, raw_output = reasoner.reasoner.conformance_checking(history_actions)
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
            # Always query for 'true' to check if the fluent holds
            # The result will be compared with expected_result later
            expected_value = 'true'
            
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
                actions = [a.strip() for a in row['actions'].split(',') if a.strip()]
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
        
        # Extract BPMN metrics
        try:
            metrics = extract_bpmn_metrics(bpmn_file)
            metrics_dict = metrics.to_dict()
            metrics_dict['model_name'] = model_name
            self.bpmn_metrics.append(metrics_dict)
        except Exception as e:
            print(f"  Warning: Could not extract BPMN metrics: {e}")
        
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
            # Check if this sample was already evaluated (for resume mode)
            sample_key = (model_name, sample['sample_id'])
            if sample_key in self.evaluated_samples:
                print(f"\n  Sample {sample['sample_id']} - SKIPPED (already evaluated)")
                continue
            
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
                # Map result to boolean based on task type:
                # - legality/projection/verification: success = True, failure = False
                # - conformance: conforms = True, not_conforms = False
                if task_type == 'conformance':
                    actual_result = metrics.result == 'conforms'
                else:
                    actual_result = metrics.result == 'success'
                
                is_correct = actual_result == sample['expected_result']
                
                if is_correct:
                    correct += 1
                    print(f"    Correct (result: {metrics.result})")
                else:
                    print(f"    Incorrect (result: {metrics.result}, expected: {sample['expected_result']})")
                
                # Store result
                result = {
                    'model_name': model_name,
                    'task_type': task_type,
                    'sample_id': sample['sample_id'],
                    'expected': sample['expected_result'],
                    'returned': actual_result,
                    'correct': is_correct,
                    'reasoning_time': metrics.reasoning_time,
                    'inferences': metrics.inferences
                }
                self.results.append(result)
                
                # Save immediately to CSV
                self._append_result_to_file(result)
            else:
                print(f"    Task failed (no metrics)")
        
        accuracy = (correct / total * 100) if total > 0 else 0
        print(f"\n  Model Accuracy: {correct}/{total} ({accuracy:.1f}%)")
    
    def run_evaluation(self):
        print("\n" + "="*70)
        print("EVALUATION - LEGALITY & CONFORMANCE")
        print("="*70)
        
        # Initialize results file
        self._initialize_results_file('_leg_conf')
        
        csv_path = os.path.join(self.output_dir, 'datasets', 'samples_leg_conf.csv')
        print(f"\nLoading samples from: {csv_path}")
        samples_by_model = self.load_samples_from_csv(csv_path)
        print(f"Loaded {sum(len(s) for s in samples_by_model.values())} samples for {len(samples_by_model)} models")

        for model_name, samples in sorted(samples_by_model.items()):
            self.evaluate_model(model_name, samples)
        self.save_results('_leg_conf')
    
    def run_projection_verification_evaluation(self):
        print("\n" + "="*70)
        print("EVALUATION - PROJECTION & PROPERTY VERIFICATION")
        print("="*70)
        
        # Initialize results file
        self._initialize_results_file('_proj_verif')
        
        csv_path = os.path.join(self.output_dir, 'datasets', 'samples_proj_verif.csv')
        print(f"\nLoading samples from: {csv_path}")
        samples_by_model = self.load_samples_from_csv(csv_path)
        print(f"Loaded {sum(len(s) for s in samples_by_model.values())} samples for {len(samples_by_model)} models")

        # Use exams-bpmn directory for these samples
        exams_dataset_dir = os.path.join(self.project_root, 'bpmn', 'exams-bpmn')
        
        for model_name, samples in sorted(samples_by_model.items()):
            self.evaluate_model_with_dataset(model_name, samples, exams_dataset_dir)
        
        self.save_results('_proj_verif')
    
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
        
        # Extract BPMN metrics
        try:
            metrics = extract_bpmn_metrics(bpmn_file)
            metrics_dict = metrics.to_dict()
            metrics_dict['model_name'] = model_name
            self.bpmn_metrics.append(metrics_dict)
        except Exception as e:
            print(f"  Warning: Could not extract BPMN metrics: {e}")
        
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
            # Check if this sample was already evaluated (for resume mode)
            sample_key = (model_name, sample['sample_id'])
            if sample_key in self.evaluated_samples:
                print(f"\n  Sample {sample['sample_id']} - SKIPPED (already evaluated)")
                continue
            
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
                # Map result to boolean based on task type:
                # - legality/projection/verification: success = True, failure = False
                # - conformance: conforms = True, not_conforms = False
                if task_type == 'conformance':
                    actual_result = metrics.result == 'conforms'
                else:
                    actual_result = metrics.result == 'success'
                
                is_correct = actual_result == sample['expected_result']
                
                if is_correct:
                    correct += 1
                    print(f"    Correct (result: {metrics.result})")
                else:
                    print(f"    Incorrect (result: {metrics.result}, expected: {sample['expected_result']})")
                
                # Store result
                result = {
                    'model_name': model_name,
                    'task_type': task_type,
                    'sample_id': sample['sample_id'],
                    'expected': sample['expected_result'],
                    'returned': actual_result,
                    'correct': is_correct,
                    'reasoning_time': metrics.reasoning_time,
                    'inferences': metrics.inferences
                }
                self.results.append(result)
                
                # Save immediately to CSV
                self._append_result_to_file(result)
            else:
                print(f"    Task failed (no metrics)")
        
        accuracy = (correct / total * 100) if total > 0 else 0
        print(f"\n  Model Accuracy: {correct}/{total} ({accuracy:.1f}%)")
    
    def save_results(self, suffix=''):
        """Save BPMN metrics, translation metrics, and finalize results file.
        
        Note: Individual results are already saved incrementally during evaluation.
        This method primarily handles BPMN and translation metrics saving and final reporting.
        """
        # Results file path is already set
        output_file = self.current_results_file
        print(f"\nResults saved to: {output_file}")
        
        # Save BPMN metrics
        if self.bpmn_metrics:
            metrics_filename = f'bpmn_metrics{suffix}.csv'
            metrics_output_file = os.path.join(self.output_dir, 'bpmn_metrics', metrics_filename)
            with open(metrics_output_file, 'w', newline='') as f:
                fieldnames = ['model_name'] + [k for k in self.bpmn_metrics[0].keys() if k != 'model_name']
                writer = csv.DictWriter(f, fieldnames=fieldnames)
                writer.writeheader()
                writer.writerows(self.bpmn_metrics)
            
            print(f"BPMN metrics saved to: {metrics_output_file}")
        
        # Save translation metrics
        if self.translation_metrics:
            trans_metrics_filename = f'translation_metrics{suffix}.csv'
            trans_metrics_output_file = os.path.join(self.output_dir, 'bpmn_metrics', trans_metrics_filename)
            with open(trans_metrics_output_file, 'w', newline='') as f:
                fieldnames = ['model_name', 'translation_time_sec', 'num_actions', 'num_fluents', 'program_size_lines']
                writer = csv.DictWriter(f, fieldnames=fieldnames)
                writer.writeheader()
                writer.writerows(self.translation_metrics)
            
            print(f"Translation metrics saved to: {trans_metrics_output_file}")
        
        return output_file
    
    def generate_summary_from_csv(self, csv_file_path: str, suffix=''):
        """Generate summary statistics from the evaluation results CSV file.
        
        Args:
            csv_file_path: Path to the evaluation results CSV file
            suffix: Suffix for the output summary file name
        """
        # Read results from CSV
        results = []
        with open(csv_file_path, 'r', newline='') as f:
            reader = csv.DictReader(f)
            for row in reader:
                results.append({
                    'task_type': row['task_type'],
                    'correct': row['correct'].lower() == 'true',
                    'reasoning_time': float(row['reasoning_time']),
                    'inferences': int(row['inferences'])
                })
        
        if not results:
            print("\nNo results to summarize.")
            return
        
        total = len(results)
        correct = sum(1 for r in results if r['correct'])
        accuracy = (correct / total * 100) if total > 0 else 0
        
        # Compute averages
        avg_time = sum(r['reasoning_time'] for r in results) / total
        avg_inferences = sum(r['inferences'] for r in results) / total
        
        # Group by task type
        legality_results = [r for r in results if r['task_type'] == 'legality']
        conformance_results = [r for r in results if r['task_type'] == 'conformance']
        projection_results = [r for r in results if r['task_type'] == 'projection']
        verification_results = [r for r in results if r['task_type'] == 'verification']
        
        legality_correct = sum(1 for r in legality_results if r['correct'])
        conformance_correct = sum(1 for r in conformance_results if r['correct'])
        projection_correct = sum(1 for r in projection_results if r['correct'])
        verification_correct = sum(1 for r in verification_results if r['correct'])
        
        legality_acc = (legality_correct / len(legality_results) * 100) if legality_results else 0
        conformance_acc = (conformance_correct / len(conformance_results) * 100) if conformance_results else 0
        projection_acc = (projection_correct / len(projection_results) * 100) if projection_results else 0
        verification_acc = (verification_correct / len(verification_results) * 100) if verification_results else 0
        
        # Compute per-task averages
        legality_time = sum(r['reasoning_time'] for r in legality_results) / len(legality_results) if legality_results else 0
        conformance_time = sum(r['reasoning_time'] for r in conformance_results) / len(conformance_results) if conformance_results else 0
        projection_time = sum(r['reasoning_time'] for r in projection_results) / len(projection_results) if projection_results else 0
        verification_time = sum(r['reasoning_time'] for r in verification_results) / len(verification_results) if verification_results else 0
        
        legality_inf = sum(r['inferences'] for r in legality_results) / len(legality_results) if legality_results else 0
        conformance_inf = sum(r['inferences'] for r in conformance_results) / len(conformance_results) if conformance_results else 0
        projection_inf = sum(r['inferences'] for r in projection_results) / len(projection_results) if projection_results else 0
        verification_inf = sum(r['inferences'] for r in verification_results) / len(verification_results) if verification_results else 0
        
        # Build summary text
        summary_lines = []
        summary_lines.append("=" * 70)
        summary_lines.append("EVALUATION SUMMARY")
        summary_lines.append("=" * 70)
        summary_lines.append(f"Total samples: {total}")
        summary_lines.append(f"Correct: {correct}")
        summary_lines.append(f"Overall Accuracy: {accuracy:.1f}%")
        summary_lines.append(f"Average Reasoning Time: {avg_time:.3f} seconds")
        summary_lines.append(f"Average Inferences: {avg_inferences:.0f}")
        summary_lines.append("")
        
        if legality_results:
            summary_lines.append(f"Legality: {legality_correct}/{len(legality_results)} ({legality_acc:.1f}%)")
            summary_lines.append(f"  Avg Time: {legality_time:.3f}s, Avg Inferences: {legality_inf:.0f}")
        if conformance_results:
            summary_lines.append(f"Conformance: {conformance_correct}/{len(conformance_results)} ({conformance_acc:.1f}%)")
            summary_lines.append(f"  Avg Time: {conformance_time:.3f}s, Avg Inferences: {conformance_inf:.0f}")
        if projection_results:
            summary_lines.append(f"Projection: {projection_correct}/{len(projection_results)} ({projection_acc:.1f}%)")
            summary_lines.append(f"  Avg Time: {projection_time:.3f}s, Avg Inferences: {projection_inf:.0f}")
        if verification_results:
            summary_lines.append(f"Property Verification: {verification_correct}/{len(verification_results)} ({verification_acc:.1f}%)")
            summary_lines.append(f"  Avg Time: {verification_time:.3f}s, Avg Inferences: {verification_inf:.0f}")
        
        summary_lines.append("=" * 70)
        
        # Print to console
        print("\n" + "\n".join(summary_lines))
        
        # Save to file
        if self.current_datetime_str:
            summary_filename = f'evaluation_summary{suffix}_{self.current_datetime_str}.txt'
            summary_output_file = os.path.join(self.output_dir, 'results', summary_filename)
            with open(summary_output_file, 'w') as f:
                f.write("\n".join(summary_lines) + "\n")
            print(f"\nSummary saved to: {summary_output_file}")
    
    def assess_results(self, csv_file_path: str, suffix: str):
        """Assess results from a specific CSV file and generate summary.
        
        Args:
            csv_file_path: Path to the evaluation results CSV file
            suffix: Suffix for the output summary file (e.g., '_leg_conf')
        """
        if not os.path.exists(csv_file_path):
            print(f"Error: CSV file not found: {csv_file_path}")
            return
        
        # Extract timestamp from CSV filename if possible
        basename = os.path.basename(csv_file_path)
        # Try to extract timestamp from filename like: evaluation_results_leg_conf_2025-11-10_09:44:50.csv
        import re
        match = re.search(r'(\d{4}-\d{2}-\d{2}_\d{2}:\d{2}:\d{2})', basename)
        if match:
            self.current_datetime_str = match.group(1)
        else:
            # Use current timestamp if we can't extract it
            self.current_datetime_str = datetime.datetime.now().strftime('%Y-%m-%d_%H:%M:%S')
        
        self.generate_summary_from_csv(csv_file_path, suffix)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='BPMN Evaluation - Legality, Conformance, Projection, and Property Verification')
    parser.add_argument('--mode', type=str, default='test', 
                       choices=['test', 'assess'],
                       help='Mode: test (run evaluation) or assess (generate summary from existing CSV)')
    parser.add_argument('--task', type=str, default='all', 
                       choices=['legality_conformance', 'projection_verification', 'all'],
                       help='Which evaluation to run/assess: legality_conformance, projection_verification, or all (default: all)')
    parser.add_argument('--csv', type=str, default=None,
                       help='Path to CSV file for assessment mode (optional, will find latest if not provided)')
    parser.add_argument('--resume', type=str, default=None,
                       help='Path to incomplete CSV file to resume evaluation from (test mode only)')
    args = parser.parse_args()
    
    project_root = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..'))
    dataset_dir = os.path.join(project_root, 'bpmn', 'dataset', 'processed')
    output_dir = os.path.join(project_root, 'evaluation')
    
    # Handle resume CSV path - convert relative to absolute if needed
    resume_csv = args.resume
    if resume_csv and not os.path.isabs(resume_csv):
        # If relative path, assume it's in the results directory
        resume_csv = os.path.join(output_dir, 'results', resume_csv)
    
    evaluator = BPMNEvaluator(dataset_dir, output_dir, resume_csv=resume_csv)
    
    if args.mode == 'test':
        # Run evaluation tests
        if args.task in ['legality_conformance', 'all']:
            evaluator.run_evaluation()
            
        if args.task in ['projection_verification', 'all']:
            # Reset results and metrics for separate evaluation
            if args.task == 'all':
                evaluator.results = []
                evaluator.bpmn_metrics = []
            evaluator.run_projection_verification_evaluation()
    
    elif args.mode == 'assess':
        # Assess existing results
        results_dir = os.path.join(output_dir, 'results')
        
        if args.task in ['legality_conformance', 'all']:
            if args.csv:
                csv_file = args.csv
            else:
                # Find latest legality_conformance CSV
                import glob
                csv_files = glob.glob(os.path.join(results_dir, 'evaluation_results_leg_conf_*.csv'))
                if not csv_files:
                    print("Error: No legality_conformance CSV files found in results directory")
                else:
                    csv_file = max(csv_files, key=os.path.getmtime)
                    print(f"Assessing latest legality_conformance results: {csv_file}")
                    evaluator.assess_results(csv_file, '_leg_conf')
        
        if args.task in ['projection_verification', 'all']:
            if args.csv and args.task == 'projection_verification':
                csv_file = args.csv
            else:
                # Find latest projection_verification CSV
                import glob
                csv_files = glob.glob(os.path.join(results_dir, 'evaluation_results_proj_verif_*.csv'))
                if not csv_files:
                    print("Error: No projection_verification CSV files found in results directory")
                else:
                    csv_file = max(csv_files, key=os.path.getmtime)
                    print(f"Assessing latest projection_verification results: {csv_file}")
                    evaluator.assess_results(csv_file, '_proj_verif')
