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
    
    def _extract_process_number(self, model_name: str) -> int:
        match = re.search(r'process_(\d+)', model_name)
        if match:
            return int(match.group(1))
        return 0
    
    def _aggregate_by_model_and_task(self):
        # Structure: task_type -> process_num -> list of samples
        aggregated = defaultdict(lambda: defaultdict(list))
        
        # Determine if this is leg_conf dataset (starts from process_1, not process_0)
        is_leg_conf = any('leg_conf' in self.results_csv_path.lower() for _ in [1])
        
        for row in self.results:
            task_type = row['task_type']
            model_name = row['model_name']
            process_num = self._extract_process_number(model_name)
            
            # Adjust process number for leg_conf dataset to start from 0
            if is_leg_conf:
                process_num -= 1
            
            correct = row['correct'].lower() == 'true'
            reasoning_time = float(row['reasoning_time'])
            inferences = int(row['inferences'])
            
            aggregated[task_type][process_num].append({
                'correct': correct,
                'reasoning_time': reasoning_time,
                'inferences': inferences
            })
        
        metrics = defaultdict(lambda: defaultdict(dict))
        for task_type, models in aggregated.items():
            for process_num, samples in models.items():
                total_samples = len(samples)
                correct_samples = sum(1 for s in samples if s['correct'])
                accuracy = (correct_samples / total_samples * 100) if total_samples > 0 else 0
                avg_reasoning_time = np.mean([s['reasoning_time'] for s in samples])
                avg_inferences = np.mean([s['inferences'] for s in samples])
                
                metrics[task_type][process_num] = {
                    'accuracy': accuracy,
                    'reasoning_time': avg_reasoning_time,
                    'inferences': avg_inferences
                }
        
        return metrics
    
    def _get_sorted_data(self, metrics: dict, task_types: list, metric_name: str):
        plot_data = {}
        for task_type in task_types:
            if task_type not in metrics:
                continue
            
            process_nums = sorted(metrics[task_type].keys())
            metric_values = [metrics[task_type][pn][metric_name] for pn in process_nums]
            plot_data[task_type] = (process_nums, metric_values)
        
        return plot_data
    
    def _create_line_chart(self, plot_data: dict, ylabel: str, title: str, filename: str):
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
        
        plt.xlabel('Process Model ID', fontsize=12, fontweight='bold')
        plt.ylabel(ylabel, fontsize=12, fontweight='bold')
        plt.title(title, fontsize=14, fontweight='bold')
        plt.grid(True, alpha=0.3, linestyle='--')
        plt.legend(fontsize=11, loc='best')
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
        
        metrics = self._aggregate_by_model_and_task()
        task_types = sorted(metrics.keys())
        if not task_types:
            print("  No task types found in data")
            return
        print(f"  Found task types: {', '.join(task_types)}")
        
        plot_data = self._get_sorted_data(metrics, task_types, 'accuracy')
        self._create_line_chart(
            plot_data,
            ylabel='Accuracy (%)',
            title=f'Accuracy vs Process Model ID{dataset_suffix}',
            filename=f'{csv_basename}_accuracy.png'
        )
        plot_data = self._get_sorted_data(metrics, task_types, 'reasoning_time')
        self._create_line_chart(
            plot_data,
            ylabel='Reasoning Time (seconds)',
            title=f'Reasoning Time vs Process Model ID{dataset_suffix}',
            filename=f'{csv_basename}_reasoning_time.png'
        )
        plot_data = self._get_sorted_data(metrics, task_types, 'inferences')
        self._create_line_chart(
            plot_data,
            ylabel='Number of Inferences',
            title=f'Inferences vs Process Model ID{dataset_suffix}',
            filename=f'{csv_basename}_inferences.png'
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
                'filename': 'translation_time_leg_conf.png',
                'adjust_offset': True,  # Start from 0 instead of 1
                'dataset_suffix': ' - Textual BPMN Dataset'
            },
            'Projection & Verification': {
                'csv': os.path.join(metrics_dir, 'translation_metrics_exams.csv'),
                'filename': 'translation_time_proj_verif.png',
                'adjust_offset': False,  # Keep original numbering
                'dataset_suffix': ' - Exams BPMN Dataset'
            }
        }
        
        # Generate a separate chart for each dataset
        for dataset_name, file_info in translation_files.items():
            csv_path = file_info['csv']
            
            if not os.path.exists(csv_path):
                print(f"  Warning: Translation metrics file not found: {csv_path}")
                continue
            
            # Read translation metrics
            process_nums = []
            translation_times = []
            
            with open(csv_path, 'r') as f:
                reader = csv.DictReader(f)
                for row in reader:
                    process_num = self._extract_process_number(row['model_name'])
                    
                    # Adjust process number for leg_conf dataset to start from 0
                    if file_info['adjust_offset']:
                        process_num -= 1
                    
                    translation_time = float(row['translation_time_sec'])
                    process_nums.append(process_num)
                    translation_times.append(translation_time)
            
            if not process_nums:
                print(f"  Warning: No data found in {csv_path}")
                continue
            
            # Sort by process number
            sorted_pairs = sorted(zip(process_nums, translation_times))
            process_nums, translation_times = zip(*sorted_pairs)
            
            print(f"  Loaded {len(process_nums)} translation metrics from {dataset_name}")
            
            # Create the chart for this dataset
            plt.figure(figsize=(12, 7))
            
            plt.plot(process_nums, translation_times, 
                    linewidth=2,
                    marker='o',
                    markersize=5,
                    color='#2E86AB',
                    label=dataset_name)
            
            plt.xlabel('Process Model ID', fontsize=12, fontweight='bold')
            plt.ylabel('Translation Time (seconds)', fontsize=12, fontweight='bold')
            plt.title(f'BPMN to IndiGolog Translation Time{file_info["dataset_suffix"]}', fontsize=14, fontweight='bold')
            plt.grid(True, alpha=0.3, linestyle='--')
            
            # Format y-axis to show 3 decimal places
            from matplotlib.ticker import FormatStrFormatter
            plt.gca().yaxis.set_major_formatter(FormatStrFormatter('%.3f'))
            
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
    
    # Generate translation time charts (once, independent of CSV files)
    if csv_files:
        # Use the first CSV file just to initialize the generator
        csv_path = os.path.join(results_dir, csv_files[0]) if not os.path.isabs(csv_files[0]) else csv_files[0]
        generator = ChartGenerator(csv_path, output_dir)
        generator.generate_translation_time_charts()


if __name__ == '__main__':
    main()
