import argparse
import csv
import numpy as np
import os


class ReportGenerator:
    def __init__(self, results_csv_path: str, output_dir: str):
        self.results_csv_path = results_csv_path
        self.output_dir = output_dir
        self.results = []
        self._load_results()
    
    def _load_results(self):
        if not os.path.exists(self.results_csv_path):
            print(f"Warning: Results file not found: {self.results_csv_path}")
            return
        
        with open(self.results_csv_path, 'r') as f:
            reader = csv.DictReader(f)
            self.results = list(reader)
        
        print(f"Loaded {len(self.results)} result entries from {self.results_csv_path}")
    
    def generate_all_reports(self):
        print("\nGenerating statistics reports...")
        self.generate_statistics_table()
        print(f"\nReports saved to: {self.output_dir}")
    
    def generate_statistics_table(self):
        output_path = os.path.join(self.output_dir, 'statistics_table.txt')
        
        with open(output_path, 'w') as f:
            f.write("="*80 + "\n")
            f.write("EVALUATION STATISTICS TABLE\n")
            f.write("="*80 + "\n\n")
            
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
    parser = argparse.ArgumentParser(
        description="Generate reports and visualizations from evaluation results"
    )
    parser.add_argument(
        '--results-csv',
        default='../results/detailed_results.csv',
        help='Path to detailed results CSV file'
    )
    parser.add_argument('--output-dir', default='..', help='Path to output directory')
    args = parser.parse_args()
    
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
