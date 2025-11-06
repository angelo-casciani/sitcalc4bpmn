"""
Report Generator and Visualization Module

This module generates visualizations and reports from evaluation results:
- Plots: reasoning time vs model size, task completion time per reasoning type
- Tables: per-model and aggregate statistics
- Charts: accuracy metrics comparison
"""

import os
import csv
import matplotlib.pyplot as plt
import matplotlib
matplotlib.use('Agg')  # Use non-interactive backend
import numpy as np


class ReportGenerator:
    """Generate reports and visualizations from evaluation results."""
    
    def __init__(self, results_csv_path: str, output_dir: str):
        """Initialize the report generator.
        
        Args:
            results_csv_path: Path to detailed results CSV
            output_dir: Directory for output files
        """
        self.results_csv_path = results_csv_path
        self.output_dir = output_dir
        self.results = []
        
        # Create plots directory
        self.plots_dir = os.path.join(output_dir, 'plots')
        os.makedirs(self.plots_dir, exist_ok=True)
        
        # Load results
        self._load_results()
    
    def _load_results(self):
        """Load results from CSV file."""
        if not os.path.exists(self.results_csv_path):
            print(f"Warning: Results file not found: {self.results_csv_path}")
            return
        
        with open(self.results_csv_path, 'r') as f:
            reader = csv.DictReader(f)
            self.results = list(reader)
        
        print(f"Loaded {len(self.results)} result entries from {self.results_csv_path}")
    
    def generate_all_reports(self):
        """Generate all reports and visualizations."""
        print("\nGenerating visualizations and reports...")
        
        self.plot_reasoning_time_vs_model_size()
        self.plot_task_completion_time()
        self.plot_accuracy_by_task_type()
        self.plot_inference_distribution()
        self.plot_translation_metrics()
        self.generate_statistics_table()
        
        print(f"\nAll plots saved to: {self.plots_dir}")
    
    def plot_reasoning_time_vs_model_size(self):
        """Plot reasoning time vs model size (total elements)."""
        # Aggregate by model
        model_data = {}
        
        for row in self.results:
            model_name = row.get('model_name', '')
            if model_name not in model_data:
                model_data[model_name] = {
                    'size': int(row.get('num_tasks', 0)) + 
                           int(row.get('num_exclusive_gateways', 0)) + 
                           int(row.get('num_parallel_gateways', 0)) + 
                           int(row.get('num_events', 0)),
                    'reasoning_times': []
                }
            
            reasoning_time = float(row.get('reasoning_time_sec', 0))
            if reasoning_time > 0:
                model_data[model_name]['reasoning_times'].append(reasoning_time)
        
        # Calculate average reasoning time per model
        sizes = []
        avg_times = []
        model_names = []
        
        for model_name, data in model_data.items():
            if data['reasoning_times']:
                sizes.append(data['size'])
                avg_times.append(np.mean(data['reasoning_times']))
                model_names.append(model_name)
        
        if not sizes:
            print("  Skipping reasoning time vs model size plot (no data)")
            return
        
        # Create plot
        plt.figure(figsize=(10, 6))
        plt.scatter(sizes, avg_times, alpha=0.6, s=100)
        
        # Add trend line
        if len(sizes) > 1:
            z = np.polyfit(sizes, avg_times, 1)
            p = np.poly1d(z)
            plt.plot(sizes, p(sizes), "r--", alpha=0.8, label='Trend')
        
        plt.xlabel('Model Size (Total Elements)', fontsize=12)
        plt.ylabel('Average Reasoning Time (seconds)', fontsize=12)
        plt.title('Reasoning Time vs Model Size', fontsize=14, fontweight='bold')
        plt.grid(True, alpha=0.3)
        plt.legend()
        plt.tight_layout()
        
        output_path = os.path.join(self.plots_dir, 'reasoning_time_vs_model_size.png')
        plt.savefig(output_path, dpi=300)
        plt.close()
        
        print(f"  ✓ Generated: reasoning_time_vs_model_size.png")
    
    def plot_task_completion_time(self):
        """Plot task completion time per reasoning type."""
        # Group by task type
        task_times = {}
        
        for row in self.results:
            task_type = row.get('task_type', '')
            reasoning_time = float(row.get('reasoning_time_sec', 0))
            
            if task_type and reasoning_time > 0:
                if task_type not in task_times:
                    task_times[task_type] = []
                task_times[task_type].append(reasoning_time)
        
        if not task_times:
            print("  Skipping task completion time plot (no data)")
            return
        
        # Create box plot
        plt.figure(figsize=(10, 6))
        
        task_types = list(task_times.keys())
        times_data = [task_times[tt] for tt in task_types]
        
        bp = plt.boxplot(times_data, labels=task_types, patch_artist=True)
        
        # Color boxes
        colors = ['#ff9999', '#66b3ff', '#99ff99', '#ffcc99']
        for patch, color in zip(bp['boxes'], colors):
            patch.set_facecolor(color)
        
        plt.ylabel('Reasoning Time (seconds)', fontsize=12)
        plt.xlabel('Task Type', fontsize=12)
        plt.title('Task Completion Time by Reasoning Type', fontsize=14, fontweight='bold')
        plt.xticks(rotation=15, ha='right')
        plt.grid(True, alpha=0.3, axis='y')
        plt.tight_layout()
        
        output_path = os.path.join(self.plots_dir, 'task_completion_time.png')
        plt.savefig(output_path, dpi=300)
        plt.close()
        
        print(f"  ✓ Generated: task_completion_time.png")
    
    def plot_accuracy_by_task_type(self):
        """Plot accuracy metrics by task type."""
        # Aggregate accuracy by task type
        task_accuracy = {}
        
        for row in self.results:
            task_type = row.get('task_type', '')
            expected = row.get('expected', '')
            actual = row.get('actual', '')
            
            if task_type and expected and actual:
                if task_type not in task_accuracy:
                    task_accuracy[task_type] = {'correct': 0, 'total': 0}
                
                task_accuracy[task_type]['total'] += 1
                if expected == actual or self._results_match(expected, actual):
                    task_accuracy[task_type]['correct'] += 1
        
        if not task_accuracy:
            print("  Skipping accuracy plot (no data)")
            return
        
        # Calculate accuracy percentages
        task_types = list(task_accuracy.keys())
        accuracies = [
            (task_accuracy[tt]['correct'] / task_accuracy[tt]['total']) * 100
            for tt in task_types
        ]
        
        # Create bar plot
        plt.figure(figsize=(10, 6))
        
        colors = ['#ff9999', '#66b3ff', '#99ff99', '#ffcc99']
        bars = plt.bar(task_types, accuracies, color=colors[:len(task_types)])
        
        # Add value labels on bars
        for bar in bars:
            height = bar.get_height()
            plt.text(bar.get_x() + bar.get_width()/2., height,
                    f'{height:.1f}%',
                    ha='center', va='bottom', fontsize=10)
        
        plt.ylabel('Accuracy (%)', fontsize=12)
        plt.xlabel('Task Type', fontsize=12)
        plt.title('Accuracy by Task Type', fontsize=14, fontweight='bold')
        plt.xticks(rotation=15, ha='right')
        plt.ylim(0, 110)
        plt.grid(True, alpha=0.3, axis='y')
        plt.tight_layout()
        
        output_path = os.path.join(self.plots_dir, 'accuracy_by_task_type.png')
        plt.savefig(output_path, dpi=300)
        plt.close()
        
        print(f"  ✓ Generated: accuracy_by_task_type.png")
    
    def plot_inference_distribution(self):
        """Plot distribution of inferences by task type."""
        task_inferences = {}
        
        for row in self.results:
            task_type = row.get('task_type', '')
            inferences = int(row.get('inferences', 0))
            
            if task_type and inferences > 0:
                if task_type not in task_inferences:
                    task_inferences[task_type] = []
                task_inferences[task_type].append(inferences)
        
        if not task_inferences:
            print("  Skipping inference distribution plot (no data)")
            return
        
        # Create violin plot
        plt.figure(figsize=(10, 6))
        
        task_types = list(task_inferences.keys())
        inferences_data = [task_inferences[tt] for tt in task_types]
        
        parts = plt.violinplot(inferences_data, positions=range(len(task_types)),
                              showmeans=True, showmedians=True)
        
        plt.xticks(range(len(task_types)), task_types, rotation=15, ha='right')
        plt.ylabel('Number of Inferences', fontsize=12)
        plt.xlabel('Task Type', fontsize=12)
        plt.title('Inference Distribution by Task Type', fontsize=14, fontweight='bold')
        plt.grid(True, alpha=0.3, axis='y')
        plt.tight_layout()
        
        output_path = os.path.join(self.plots_dir, 'inference_distribution.png')
        plt.savefig(output_path, dpi=300)
        plt.close()
        
        print(f"  ✓ Generated: inference_distribution.png")
    
    def plot_translation_metrics(self):
        """Plot translation metrics (time and memory)."""
        # Aggregate by model (take first entry for each model)
        model_translation = {}
        
        for row in self.results:
            model_name = row.get('model_name', '')
            if model_name and model_name not in model_translation:
                model_translation[model_name] = {
                    'time': round(float(row.get('translation_time_sec', 0)), 3),
                    'memory': round(float(row.get('memory_usage_mb', 0)), 3),
                    'actions': int(row.get('num_actions', 0))
                }
        
        if not model_translation:
            print("  Skipping translation metrics plot (no data)")
            return
        
        # Create subplot for time and memory
        fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14, 6))
        
        model_names = list(model_translation.keys())
        times = [model_translation[m]['time'] for m in model_names]
        memories = [model_translation[m]['memory'] for m in model_names]
        
        # Translation time
        ax1.bar(range(len(model_names)), times, color='#66b3ff')
        ax1.set_ylabel('Translation Time (seconds)', fontsize=11)
        ax1.set_xlabel('Model', fontsize=11)
        ax1.set_title('Translation Time per Model', fontsize=12, fontweight='bold')
        ax1.set_xticks(range(len(model_names)))
        ax1.set_xticklabels(model_names, rotation=45, ha='right', fontsize=8)
        ax1.grid(True, alpha=0.3, axis='y')
        
        # Memory usage
        ax2.bar(range(len(model_names)), memories, color='#ff9999')
        ax2.set_ylabel('Memory Usage (MB)', fontsize=11)
        ax2.set_xlabel('Model', fontsize=11)
        ax2.set_title('Memory Usage per Model', fontsize=12, fontweight='bold')
        ax2.set_xticks(range(len(model_names)))
        ax2.set_xticklabels(model_names, rotation=45, ha='right', fontsize=8)
        ax2.grid(True, alpha=0.3, axis='y')
        
        plt.tight_layout()
        
        output_path = os.path.join(self.plots_dir, 'translation_metrics.png')
        plt.savefig(output_path, dpi=300)
        plt.close()
        
        print(f"  ✓ Generated: translation_metrics.png")
    
    def generate_statistics_table(self):
        """Generate a statistics table in TXT format."""
        output_path = os.path.join(self.output_dir, 'statistics_table.txt')
        
        with open(output_path, 'w') as f:
            f.write("="*80 + "\n")
            f.write("EVALUATION STATISTICS TABLE\n")
            f.write("="*80 + "\n\n")
            
            # Per-task type statistics
            task_stats = {}
            
            for row in self.results:
                task_type = row.get('task_type', '')
                if not task_type:
                    continue
                
                if task_type not in task_stats:
                    task_stats[task_type] = {
                        'times': [],
                        'inferences': [],
                        'correct': 0,
                        'total': 0
                    }
                
                task_stats[task_type]['times'].append(float(row.get('reasoning_time_sec', 0)))
                task_stats[task_type]['inferences'].append(int(row.get('inferences', 0)))
                task_stats[task_type]['total'] += 1
                
                if self._results_match(row.get('expected', ''), row.get('actual', '')):
                    task_stats[task_type]['correct'] += 1
            
            f.write("Task Type Statistics\n")
            f.write("-"*80 + "\n")
            f.write(f"{'Task Type':<25} {'Avg Time (s)':<15} {'Avg Inferences':<18} {'Accuracy':<10}\n")
            f.write("-"*80 + "\n")
            
            for task_type, stats in task_stats.items():
                avg_time = np.mean(stats['times']) if stats['times'] else 0
                avg_inferences = np.mean(stats['inferences']) if stats['inferences'] else 0
                accuracy = (stats['correct'] / stats['total'] * 100) if stats['total'] > 0 else 0
                
                f.write(f"{task_type:<25} {avg_time:<15.3f} {avg_inferences:<18.0f} {accuracy:<10.1f}%\n")
            
            f.write("\n")
        
        print(f"  ✓ Generated: statistics_table.txt")
    
    def _results_match(self, expected: str, actual: str) -> bool:
        """Check if expected and actual results match.
        
        Args:
            expected: Expected result string
            actual: Actual result string
            
        Returns:
            True if results match
        """
        positive = ['success', 'true', 'conforms']
        negative = ['failure', 'false', 'not_conforms']
        
        expected_lower = expected.lower()
        actual_lower = actual.lower()
        
        expected_positive = any(p in expected_lower for p in positive)
        actual_positive = any(p in actual_lower for p in positive)
        
        return expected_positive == actual_positive


def main():
    """Main entry point for report generation."""
    import argparse
    
    parser = argparse.ArgumentParser(
        description="Generate reports and visualizations from evaluation results"
    )
    parser.add_argument(
        '--results-csv',
        default='../results/detailed_results.csv',
        help='Path to detailed results CSV file'
    )
    parser.add_argument(
        '--output-dir',
        default='..',
        help='Path to output directory'
    )
    
    args = parser.parse_args()
    
    # Resolve paths
    script_dir = os.path.dirname(os.path.abspath(__file__))
    results_csv = os.path.abspath(os.path.join(script_dir, args.results_csv))
    output_dir = os.path.abspath(os.path.join(script_dir, args.output_dir))
    
    generator = ReportGenerator(results_csv, output_dir)
    generator.generate_all_reports()
    
    print("\n" + "="*70)
    print("REPORT GENERATION COMPLETE")
    print("="*70)


if __name__ == '__main__':
    main()
