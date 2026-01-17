import argparse
from collections import defaultdict
import csv
import matplotlib.pyplot as plt
import matplotlib
matplotlib.use('Agg')
import numpy as np
import os
import re


class ChartGenerator:
    def __init__(self, results_csv_path: str, output_dir: str):
        self.results_csv_path = results_csv_path
        self.output_dir = output_dir
        self.results = []
        self.charts_dir = os.path.join(output_dir, 'results', 'charts')
        os.makedirs(self.charts_dir, exist_ok=True)
        # Get project root - output_dir is typically 'evaluation/', so parent is project root
        self.project_root = os.path.abspath(output_dir)
        self._load_results()
    
    def _load_results(self):
        if not os.path.exists(self.results_csv_path):
            print(f"Error: Results file not found: {self.results_csv_path}")
            return
        
        with open(self.results_csv_path, 'r') as f:
            reader = csv.DictReader(f)
            self.results = list(reader)
        
        print(f"Loaded {len(self.results)} result entries from {self.results_csv_path}")
    
    def _load_bpmn_metrics(self, metrics_csv_path: str) -> dict:
        """Load BPMN metrics and return mapping from model_name to total_elements."""
        model_to_elements = {}
        if not os.path.exists(metrics_csv_path):
            print(f"Warning: BPMN metrics file not found: {metrics_csv_path}")
            return model_to_elements
        
        with open(metrics_csv_path, 'r') as f:
            reader = csv.DictReader(f)
            for row in reader:
                model_name = row['model_name']
                total_elements = int(row['total_elements'])
                model_to_elements[model_name] = total_elements
        
        return model_to_elements
    
    def _extract_process_number(self, model_name: str) -> int:
        match = re.search(r'process_(\d+)', model_name)
        if match:
            return int(match.group(1))
        return 0
    
    def _aggregate_by_model_and_task(self):
        # Determine which BPMN metrics file to use
        is_leg_conf = 'leg_conf' in self.results_csv_path.lower()
        metrics_dir = os.path.join(self.output_dir, 'bpmn_metrics')
        
        if is_leg_conf:
            bpmn_metrics_csv = os.path.join(metrics_dir, 'bpmn_metrics_processed.csv')
        else:
            bpmn_metrics_csv = os.path.join(metrics_dir, 'bpmn_metrics_exams.csv')
        
        # Load mapping from model_name to total_elements
        model_to_elements = self._load_bpmn_metrics(bpmn_metrics_csv)
        
        # Structure: task_type -> total_elements -> list of samples
        aggregated = defaultdict(lambda: defaultdict(list))
        
        # Also collect timeout/error markers per task and complexity
        timeouts_by_task = defaultdict(set)
        errors_by_task = defaultdict(set)

        for row in self.results:
            task_type = row['task_type']
            model_name = row['model_name']
            
            # Get total_elements for this model
            if model_name not in model_to_elements:
                print(f"Warning: No BPMN metrics found for {model_name}, skipping")
                continue
            
            total_elements = model_to_elements[model_name]
            
            # Record timeout/error presence for markers, but exclude from metric aggregation
            status = row.get('status', '').strip().lower()
            if status == 'timeout':
                timeouts_by_task[task_type].add(total_elements)
                # do not include timed-out rows in aggregated metrics
                continue
            if status == 'error':
                errors_by_task[task_type].add(total_elements)
                # do not include error rows in aggregated metrics
                continue
            correct = row['correct'].lower() == 'true'
            reasoning_time = float(row['reasoning_time'])
            inferences = int(row['inferences'])
            
            aggregated[task_type][total_elements].append({
                'correct': correct,
                'reasoning_time': reasoning_time * 1000,  # Convert to milliseconds
                'inferences': inferences
            })
        
        # Calculate average metrics for each task_type and total_elements
        # Multiple models with same total_elements will be averaged
        metrics = defaultdict(lambda: defaultdict(dict))
        for task_type, complexity_groups in aggregated.items():
            for total_elements, samples in complexity_groups.items():
                total_samples = len(samples)
                correct_samples = sum(1 for s in samples if s['correct'])
                accuracy = (correct_samples / total_samples * 100) if total_samples > 0 else 0
                avg_reasoning_time = np.mean([s['reasoning_time'] for s in samples])
                avg_inferences = np.mean([s['inferences'] for s in samples])
                
                metrics[task_type][total_elements] = {
                    'accuracy': accuracy,
                    'reasoning_time': avg_reasoning_time,
                    'inferences': avg_inferences
                }
        
        return metrics, timeouts_by_task, errors_by_task
    
    def _get_sorted_data(self, metrics: dict, task_types: list, metric_name: str):
        plot_data = {}
        for task_type in task_types:
            if task_type not in metrics:
                continue
            
            # Sort by total_elements (complexity)
            total_elements_sorted = sorted(metrics[task_type].keys())
            metric_values = [metrics[task_type][te][metric_name] for te in total_elements_sorted]
            plot_data[task_type] = (total_elements_sorted, metric_values)
        
        return plot_data
    
    def _create_line_chart(self, plot_data: dict, ylabel: str, title: str, filename: str,
                           timeouts_by_task: dict = None, errors_by_task: dict = None):
        if not plot_data:
            print(f"  Skipping {title} (no data)")
            return
        
        plt.figure(figsize=(12, 7))        
        for idx, (task_type, (x_vals, y_vals)) in enumerate(plot_data.items()):
            plt.plot(x_vals, y_vals, 
                    linewidth=2,
                    marker='o',
                    markersize=5,
                    label=task_type.capitalize())
        
        plt.xlabel('Model Complexity (Total Elements)', fontsize=16, fontweight='bold')
        plt.ylabel(ylabel, fontsize=16, fontweight='bold')
        plt.title(title, fontsize=18, fontweight='bold')
        plt.grid(True, alpha=0.3, linestyle='--')
        plt.legend(fontsize=14, loc='best')
        plt.xticks(fontsize=14)
        plt.yticks(fontsize=14)
        # If we have timeout markers, plot them as red 'x' at y=0
        any_timeouts = False
        if timeouts_by_task:
            timeout_x = []
            timeout_y = []
            for task_type, process_set in timeouts_by_task.items():
                for proc in process_set:
                    timeout_x.append(proc)
                    timeout_y.append(0)
            if timeout_x:
                any_timeouts = True
                plt.scatter(timeout_x, timeout_y, marker='x', color='red', s=80, linewidths=2, zorder=20, label='Timeout')

        # If there are timeouts and current y-axis doesn't start at 0, extend to 0 so markers are visible
        if any_timeouts:
            ymin, ymax = plt.ylim()
            if ymin > 0:
                plt.ylim(0, ymax)

        plt.tight_layout()
        
        output_path = os.path.join(self.charts_dir, filename)
        plt.savefig(output_path, dpi=300, bbox_inches='tight')
        plt.close()
        print(f"  Saved: {output_path}")
    
    def generate_all_charts(self):
        if not self.results:
            print("No results to plot.")
            return
        
        print(f"\nGenerating charts from {self.results_csv_path}...")
        csv_basename = os.path.splitext(os.path.basename(self.results_csv_path))[0]
        
        # Determine dataset name for title suffix
        is_leg_conf = 'leg_conf' in self.results_csv_path.lower()
        dataset_suffix = ' - Textual BPMN Dataset' if is_leg_conf else ' - Exams BPMN Dataset'
        
        metrics, timeouts_by_task, errors_by_task = self._aggregate_by_model_and_task()
        task_types = sorted(metrics.keys())
        if not task_types:
            print("  No task types found in data")
            return
        print(f"  Found task types: {', '.join(task_types)}")
        
        plot_data = self._get_sorted_data(metrics, task_types, 'accuracy')
        self._create_line_chart(
            plot_data,
            ylabel='Accuracy (%)',
            title=f'Accuracy vs Model Complexity{dataset_suffix}',
            filename=f'{csv_basename}_accuracy.png'
            , timeouts_by_task=timeouts_by_task, errors_by_task=errors_by_task
        )
        plot_data = self._get_sorted_data(metrics, task_types, 'reasoning_time')
        self._create_line_chart(
            plot_data,
            ylabel='Reasoning Time (ms)',
            title=f'Reasoning Time vs Model Complexity{dataset_suffix}',
            filename=f'{csv_basename}_reasoning_time.png'
            , timeouts_by_task=timeouts_by_task, errors_by_task=errors_by_task
        )
        plot_data = self._get_sorted_data(metrics, task_types, 'inferences')
        self._create_line_chart(
            plot_data,
            ylabel='Number of Inferences',
            title=f'Inferences vs Model Complexity{dataset_suffix}',
            filename=f'{csv_basename}_inferences.png'
            , timeouts_by_task=timeouts_by_task, errors_by_task=errors_by_task
        )
        
        print(f"\nAll charts saved to: {self.charts_dir}")
    
    def generate_translation_time_charts(self):
        """Generate separate translation time charts for both datasets."""
        print(f"\nGenerating translation time charts...")
        
        # Define paths to translation metrics files
        metrics_dir = os.path.join(self.output_dir, 'bpmn_metrics')
        translation_files = {
            'Legality & Conformance': {
                'csv': os.path.join(metrics_dir, 'translation_metrics_leg_conf.csv'),
                'bpmn_csv': os.path.join(metrics_dir, 'bpmn_metrics_processed.csv'),
                'filename': 'translation_time_leg_conf.png',
                'dataset_suffix': ' - Textual BPMN Dataset'
            },
            'Projection & Verification': {
                'csv': os.path.join(metrics_dir, 'translation_metrics_exams.csv'),
                'bpmn_csv': os.path.join(metrics_dir, 'bpmn_metrics_exams.csv'),
                'filename': 'translation_time_proj_verif.png',
                'dataset_suffix': ' - Exams BPMN Dataset'
            }
        }
        
        # Generate a separate chart for each dataset
        for dataset_name, file_info in translation_files.items():
            csv_path = file_info['csv']
            bpmn_csv_path = file_info['bpmn_csv']
            
            if not os.path.exists(csv_path):
                print(f"  Warning: Translation metrics file not found: {csv_path}")
                continue
            
            # Load BPMN metrics to get total_elements for each model
            model_to_elements = self._load_bpmn_metrics(bpmn_csv_path)
            
            # Read translation metrics and map to total_elements
            # Group by total_elements and average translation times
            complexity_to_times = defaultdict(list)
            
            with open(csv_path, 'r') as f:
                reader = csv.DictReader(f)
                for row in reader:
                    model_name = row['model_name']
                    
                    if model_name not in model_to_elements:
                        print(f"  Warning: No BPMN metrics found for {model_name}, skipping")
                        continue
                    
                    total_elements = model_to_elements[model_name]
                    translation_time = float(row['translation_time_sec']) * 1000  # Convert to ms
                    complexity_to_times[total_elements].append(translation_time)
            
            if not complexity_to_times:
                print(f"  Warning: No data found in {csv_path}")
                continue
            
            # Average translation times for same complexity levels
            total_elements_list = sorted(complexity_to_times.keys())
            avg_translation_times = [np.mean(complexity_to_times[te]) for te in total_elements_list]
            
            print(f"  Loaded {sum(len(v) for v in complexity_to_times.values())} translation metrics from {dataset_name}")
            print(f"  Aggregated into {len(total_elements_list)} unique complexity levels")
            
            # Create the chart for this dataset
            plt.figure(figsize=(12, 7))
            
            plt.plot(total_elements_list, avg_translation_times, 
                    linewidth=2,
                    marker='o',
                    markersize=5,
                    color='#2E86AB',
                    label=dataset_name)
            
            plt.xlabel('Model Complexity (Total Elements)', fontsize=16, fontweight='bold')
            plt.ylabel('Translation Time (ms)', fontsize=16, fontweight='bold')
            plt.title(f'BPMN to IndiGolog Translation Time{file_info["dataset_suffix"]}', fontsize=18, fontweight='bold')
            plt.grid(True, alpha=0.3, linestyle='--')
            plt.xticks(fontsize=14)
            plt.yticks(fontsize=14)
            
            plt.tight_layout()
            
            output_path = os.path.join(self.charts_dir, file_info['filename'])
            plt.savefig(output_path, dpi=300, bbox_inches='tight')
            plt.close()
            print(f"  Saved: {output_path}")



def main():
    parser = argparse.ArgumentParser(description='Generate line charts from evaluation results')
    parser.add_argument('--results-dir', type=str, 
                       default='../results',
                       help='Directory containing evaluation results CSV files')
    parser.add_argument('--output-dir', type=str,
                       default='..',
                       help='Directory for output charts (evaluation root directory)')
    parser.add_argument('--csv-file', type=str,
                       help='Specific CSV file to process (optional)')
    
    args = parser.parse_args()
    
    # Make paths absolute
    script_dir = os.path.dirname(os.path.abspath(__file__))
    results_dir = os.path.abspath(os.path.join(script_dir, args.results_dir))
    output_dir = os.path.abspath(os.path.join(script_dir, args.output_dir))
    
    # Process specific file or all CSV files
    if args.csv_file:
        csv_files = [args.csv_file]
    else:
        # Find all CSV files in results directory
        csv_files = [f for f in os.listdir(results_dir) 
                    if f.startswith('evaluation_results_') and f.endswith('.csv')]
    
    if not csv_files:
        print(f"No CSV files found in {results_dir}")
        return
    
    print(f"Found {len(csv_files)} CSV file(s) to process")
    
    # Generate charts for each CSV file
    for csv_file in csv_files:
        csv_path = os.path.join(results_dir, csv_file) if not os.path.isabs(csv_file) else csv_file
        
        generator = ChartGenerator(csv_path, output_dir)
        generator.generate_all_charts()
        print()
    
    if csv_files: # Generate translation time charts
        csv_path = os.path.join(results_dir, csv_files[0]) if not os.path.isabs(csv_files[0]) else csv_files[0]
        generator = ChartGenerator(csv_path, output_dir)
        generator.generate_translation_time_charts()


if __name__ == '__main__':
    main()
