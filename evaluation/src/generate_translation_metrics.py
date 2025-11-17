#!/usr/bin/env python3
"""
Generate Translation Metrics for BPMN Models

This script translates all BPMN files in specified datasets and collects
translation metrics including:
- Translation time
- Number of actions (prim_action declarations)
- Number of fluents (rel_fluent and fun_fluent declarations)
- Program size in lines

Usage:
    python generate_translation_metrics.py
"""

import os
import sys
import csv
import time
import re
from scipy import stats

eval_src_dir = os.path.dirname(os.path.abspath(__file__))
main_src_dir = os.path.abspath(os.path.join(eval_src_dir, '..', '..', 'src'))
sys.path.insert(0, eval_src_dir)
sys.path.insert(0, main_src_dir)

from translator_service import TranslatorService


def count_actions_and_fluents(prolog_code: str) -> tuple:
    """Count actions and fluents in generated Prolog code.
    
    Args:
        prolog_code: The generated Prolog code as a string
        
    Returns:
        Tuple of (num_actions, num_fluents)
    """
    # Count prim_action declarations
    # Pattern matches: prim_action(action_name(...)). or prim_action(Act) :- ...
    prim_action_pattern = r'^\s*prim_action\s*\('
    num_prim_actions = len(re.findall(prim_action_pattern, prolog_code, re.MULTILINE))
    
    # Count exog_action declarations
    # Pattern matches: exog_action(action_name(...)). or exog_action(Act) :- ...
    exog_action_pattern = r'^\s*exog_action\s*\('
    num_exog_actions = len(re.findall(exog_action_pattern, prolog_code, re.MULTILINE))
    
    # Total actions = prim_action + exog_action
    num_actions = num_prim_actions + num_exog_actions
    
    # Count rel_fluent declarations
    rel_fluent_pattern = r'^\s*rel_fluent\s*\('
    num_rel_fluents = len(re.findall(rel_fluent_pattern, prolog_code, re.MULTILINE))
    
    # Count fun_fluent declarations
    fun_fluent_pattern = r'^\s*fun_fluent\s*\('
    num_fun_fluents = len(re.findall(fun_fluent_pattern, prolog_code, re.MULTILINE))
    
    # Total fluents = rel_fluent + fun_fluent
    num_fluents = num_rel_fluents + num_fun_fluents
    
    return num_actions, num_fluents


def generate_translation_summary(metrics_list: list, csv_path: str, dataset_name: str):
    """Generate summary statistics for translation metrics.
    
    Args:
        metrics_list: List of metrics dictionaries
        csv_path: Path to the CSV file (used to determine output path)
        dataset_name: Name of the dataset for the report
    """
    import statistics
    from datetime import datetime
    
    if not metrics_list:
        return
    
    num_models = len(metrics_list)
    
    # Translation time stats
    translation_times = [m['translation_time_sec'] for m in metrics_list]
    avg_translation_time = statistics.mean(translation_times)
    median_translation_time = statistics.median(translation_times)
    min_translation_time = min(translation_times)
    max_translation_time = max(translation_times)
    stdev_translation_time = statistics.stdev(translation_times) if len(translation_times) > 1 else 0
    
    # Action stats
    num_actions_list = [m['num_actions'] for m in metrics_list]
    total_actions = sum(num_actions_list)
    avg_actions = statistics.mean(num_actions_list)
    median_actions = statistics.median(num_actions_list)
    min_actions = min(num_actions_list)
    max_actions = max(num_actions_list)
    
    # Fluent stats
    num_fluents_list = [m['num_fluents'] for m in metrics_list]
    total_fluents = sum(num_fluents_list)
    avg_fluents = statistics.mean(num_fluents_list)
    median_fluents = statistics.median(num_fluents_list)
    min_fluents = min(num_fluents_list)
    max_fluents = max(num_fluents_list)
    
    # Program size stats
    program_sizes = [m['program_size_lines'] for m in metrics_list]
    avg_program_size = statistics.mean(program_sizes)
    median_program_size = statistics.median(program_sizes)
    min_program_size = min(program_sizes)
    max_program_size = max(program_sizes)
    
    # Find models with extreme values
    fastest_model = min(metrics_list, key=lambda x: x['translation_time_sec'])
    slowest_model = max(metrics_list, key=lambda x: x['translation_time_sec'])
    most_actions_model = max(metrics_list, key=lambda x: x['num_actions'])
    most_fluents_model = max(metrics_list, key=lambda x: x['num_fluents'])
    largest_program_model = max(metrics_list, key=lambda x: x['program_size_lines'])
    
    # Generate summary report
    timestamp = datetime.now().strftime('%Y-%m-%d %H:%M:%S')
    
    summary = f"""
{'='*80}
TRANSLATION METRICS SUMMARY - {dataset_name}
{'='*80}

Generated: {timestamp}
Dataset: {dataset_name}
Source File: {os.path.basename(csv_path)}

{'='*80}
DATASET OVERVIEW
{'='*80}

Total Models Translated: {num_models}
Total Actions Generated: {total_actions}
Total Fluents Generated: {total_fluents}

{'='*80}
TRANSLATION TIME STATISTICS (seconds)
{'='*80}

Average:        {avg_translation_time:.6f} sec
Median:         {median_translation_time:.6f} sec
Std Deviation:  {stdev_translation_time:.6f} sec
Minimum:        {min_translation_time:.6f} sec ({fastest_model['model_name']})
Maximum:        {max_translation_time:.6f} sec ({slowest_model['model_name']})

{'='*80}
ACTIONS STATISTICS (prim_action declarations)
{'='*80}

Total Actions:  {total_actions}
Average:        {avg_actions:.1f} actions/model
Median:         {median_actions:.1f} actions/model
Minimum:        {min_actions} actions/model
Maximum:        {max_actions} actions/model ({most_actions_model['model_name']})

{'='*80}
FLUENTS STATISTICS (rel_fluent + fun_fluent declarations)
{'='*80}

Total Fluents:  {total_fluents}
Average:        {avg_fluents:.1f} fluents/model
Median:         {median_fluents:.1f} fluents/model
Minimum:        {min_fluents} fluents/model
Maximum:        {max_fluents} fluents/model ({most_fluents_model['model_name']})

{'='*80}
PROGRAM SIZE STATISTICS (lines of Prolog code)
{'='*80}

Average:        {avg_program_size:.1f} lines/model
Median:         {median_program_size:.1f} lines/model
Minimum:        {min_program_size} lines ({min(metrics_list, key=lambda x: x['program_size_lines'])['model_name']})
Maximum:        {max_program_size} lines ({largest_program_model['model_name']})

{'='*80}
TOP 5 MODELS BY TRANSLATION TIME
{'='*80}

"""
    
    # Add top 5 slowest models
    sorted_by_time = sorted(metrics_list, key=lambda x: x['translation_time_sec'], reverse=True)[:5]
    for i, m in enumerate(sorted_by_time, 1):
        summary += f"{i}. {m['model_name']}: {m['translation_time_sec']:.6f} sec "
        summary += f"(actions={m['num_actions']}, fluents={m['num_fluents']}, lines={m['program_size_lines']})\n"
    
    summary += f"""
{'='*80}
TOP 5 MODELS BY COMPLEXITY (actions + fluents)
{'='*80}

"""
    
    # Add top 5 most complex models
    sorted_by_complexity = sorted(metrics_list, key=lambda x: x['num_actions'] + x['num_fluents'], reverse=True)[:5]
    for i, m in enumerate(sorted_by_complexity, 1):
        complexity = m['num_actions'] + m['num_fluents']
        summary += f"{i}. {m['model_name']}: {complexity} total elements "
        summary += f"(actions={m['num_actions']}, fluents={m['num_fluents']})\n"
    
    summary += f"""
{'='*80}
TOP 5 LARGEST PROGRAMS (by lines of code)
{'='*80}

"""
    
    # Add top 5 largest programs
    sorted_by_size = sorted(metrics_list, key=lambda x: x['program_size_lines'], reverse=True)[:5]
    for i, m in enumerate(sorted_by_size, 1):
        summary += f"{i}. {m['model_name']}: {m['program_size_lines']} lines "
        summary += f"(actions={m['num_actions']}, fluents={m['num_fluents']})\n"
    
    summary += f"""
{'='*80}
CORRELATION ANALYSIS
{'='*80}

"""
    
    # Calculate correlations with p-values using scipy
    try:
        # Calculate Pearson correlation with p-values
        corr_time_actions, p_time_actions = stats.pearsonr(translation_times, num_actions_list)
        corr_time_fluents, p_time_fluents = stats.pearsonr(translation_times, num_fluents_list)
        corr_time_size, p_time_size = stats.pearsonr(translation_times, program_sizes)
        corr_actions_fluents, p_actions_fluents = stats.pearsonr(num_actions_list, num_fluents_list)
        
        summary += f"Translation Time vs Actions:      r = {corr_time_actions:+.3f}, p = {p_time_actions:.4f}\n"
        summary += f"Translation Time vs Fluents:      r = {corr_time_fluents:+.3f}, p = {p_time_fluents:.4f}\n"
        summary += f"Translation Time vs Program Size: r = {corr_time_size:+.3f}, p = {p_time_size:.4f}\n"
        summary += f"Actions vs Fluents:               r = {corr_actions_fluents:+.3f}, p = {p_actions_fluents:.4f}\n"
        
    except ImportError:
        # Fallback to simple correlation without p-values if scipy not available
        def pearson_correlation(x_list, y_list):
            n = len(x_list)
            if n < 2:
                return 0
            mean_x = statistics.mean(x_list)
            mean_y = statistics.mean(y_list)
            numerator = sum((x - mean_x) * (y - mean_y) for x, y in zip(x_list, y_list))
            denominator_x = sum((x - mean_x) ** 2 for x in x_list)
            denominator_y = sum((y - mean_y) ** 2 for y in y_list)
            if denominator_x == 0 or denominator_y == 0:
                return 0
            return numerator / (denominator_x * denominator_y) ** 0.5
        
        corr_time_actions = pearson_correlation(translation_times, num_actions_list)
        corr_time_fluents = pearson_correlation(translation_times, num_fluents_list)
        corr_time_size = pearson_correlation(translation_times, program_sizes)
        corr_actions_fluents = pearson_correlation(num_actions_list, num_fluents_list)
        
        summary += f"Translation Time vs Actions:      r = {corr_time_actions:+.3f}\n"
        summary += f"Translation Time vs Fluents:      r = {corr_time_fluents:+.3f}\n"
        summary += f"Translation Time vs Program Size: r = {corr_time_size:+.3f}\n"
        summary += f"Actions vs Fluents:               r = {corr_actions_fluents:+.3f}\n"
        summary += "\nNote: p-values not available (scipy not installed)\n"
    
    summary += f"""
{'='*80}
END OF SUMMARY
{'='*80}
"""
    
    # Write summary to file
    summary_path = csv_path.replace('.csv', '_summary.txt')
    with open(summary_path, 'w') as f:
        f.write(summary)
    
    print(f"  Summary saved to: {summary_path}")


def translate_and_measure(bpmn_path: str, model_name: str, translator: TranslatorService) -> dict:
    """Translate a BPMN file and measure metrics.
    
    Args:
        bpmn_path: Path to the BPMN file
        model_name: Name of the model (e.g., 'process_1.bpmn')
        translator: TranslatorService instance
        
    Returns:
        Dictionary with translation metrics
    """
    model_id = model_name.replace('.bpmn', '')
    
    # Measure translation time
    start_time = time.time()
    
    try:
        success, message, translated_path, prolog_code = translator.translate_bpmn_file(
            bpmn_path, model_id
        )
        
        translation_time = time.time() - start_time
        
        if not success:
            print(f"    ✗ Translation failed: {message}")
            return None
        
        # Count lines in the generated code
        program_size_lines = len(prolog_code.split('\n'))
        
        # Count actions and fluents
        num_actions, num_fluents = count_actions_and_fluents(prolog_code)
        
        return {
            'model_name': model_name,
            'translation_time_sec': translation_time,
            'num_actions': num_actions,
            'num_fluents': num_fluents,
            'program_size_lines': program_size_lines
        }
        
    except Exception as e:
        print(f"    ✗ Error: {e}")
        import traceback
        traceback.print_exc()
        return None


def generate_metrics_for_dataset(dataset_dir: str, output_file: str, dataset_name: str):
    """Generate translation metrics for all BPMN files in a dataset.
    
    Args:
        dataset_dir: Directory containing BPMN files
        output_file: Path to output CSV file
        dataset_name: Name of the dataset for logging
    """
    print(f"\n{'='*70}")
    print(f"Generating translation metrics for: {dataset_name}")
    print(f"Dataset directory: {dataset_dir}")
    print(f"{'='*70}")
    
    if not os.path.exists(dataset_dir):
        print(f"Error: Dataset directory not found: {dataset_dir}")
        return
    
    # Find all BPMN files
    bpmn_files = sorted([f for f in os.listdir(dataset_dir) if f.endswith('.bpmn')])
    
    if not bpmn_files:
        print(f"Warning: No BPMN files found in {dataset_dir}")
        return
    
    print(f"Found {len(bpmn_files)} BPMN files")
    
    # Create temporary directory for translations
    project_root = os.path.abspath(os.path.join(eval_src_dir, '..', '..'))
    temp_dir = os.path.join(project_root, 'pl_models', 'translation_metrics_temp')
    os.makedirs(temp_dir, exist_ok=True)
    
    # Initialize translator
    translator = TranslatorService(output_dir=temp_dir)
    
    # Collect metrics for all files
    all_metrics = []
    errors = []
    
    for i, bpmn_file in enumerate(bpmn_files, 1):
        bpmn_path = os.path.join(dataset_dir, bpmn_file)
        print(f"  [{i}/{len(bpmn_files)}] Processing: {bpmn_file}...", end=' ')
        
        metrics = translate_and_measure(bpmn_path, bpmn_file, translator)
        
        if metrics:
            all_metrics.append(metrics)
            print(f"✓ (actions={metrics['num_actions']}, fluents={metrics['num_fluents']}, lines={metrics['program_size_lines']})")
        else:
            errors.append(bpmn_file)
    
    # Save to CSV
    if all_metrics:
        os.makedirs(os.path.dirname(output_file), exist_ok=True)
        
        fieldnames = ['model_name', 'translation_time_sec', 'num_actions', 
                     'num_fluents', 'program_size_lines']
        
        with open(output_file, 'w', newline='') as f:
            writer = csv.DictWriter(f, fieldnames=fieldnames)
            writer.writeheader()
            writer.writerows(all_metrics)
        
        print(f"\n✓ Metrics saved to: {output_file}")
        print(f"  Successfully processed: {len(all_metrics)} models")
        
        # Print summary statistics
        total_actions = sum(m['num_actions'] for m in all_metrics)
        total_fluents = sum(m['num_fluents'] for m in all_metrics)
        avg_actions = total_actions / len(all_metrics) if all_metrics else 0
        avg_fluents = total_fluents / len(all_metrics) if all_metrics else 0
        
        print(f"\n  Summary Statistics:")
        print(f"    Total actions: {total_actions}")
        print(f"    Total fluents: {total_fluents}")
        print(f"    Average actions per model: {avg_actions:.1f}")
        print(f"    Average fluents per model: {avg_fluents:.1f}")
        
        if errors:
            print(f"\n  Failed to process: {len(errors)} models")
            for bpmn_file in errors:
                print(f"    - {bpmn_file}")
        
        # Generate summary
        generate_translation_summary(all_metrics, output_file, dataset_name)
    else:
        print("\nNo metrics collected")
    
    # Clean up temporary directory
    print(f"\nCleaning up temporary directory: {temp_dir}")
    import shutil
    if os.path.exists(temp_dir):
        shutil.rmtree(temp_dir)


def main():
    """Main function to generate metrics for all datasets."""
    # Determine project root
    project_root = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..'))
    
    # Define datasets
    datasets = [
        {
            'name': 'Legality & Conformance (Processed Dataset)',
            'dir': os.path.join(project_root, 'bpmn', 'dataset', 'processed'),
            'output': os.path.join(project_root, 'evaluation', 'bpmn_metrics', 'translation_metrics_leg_conf.csv')
        },
        {
            'name': 'Projection & Verification (Exams BPMN)',
            'dir': os.path.join(project_root, 'bpmn', 'exams-bpmn'),
            'output': os.path.join(project_root, 'evaluation', 'bpmn_metrics', 'translation_metrics_exams.csv')
        }
    ]
    
    # Process each dataset
    for dataset in datasets:
        generate_metrics_for_dataset(
            dataset_dir=dataset['dir'],
            output_file=dataset['output'],
            dataset_name=dataset['name']
        )
    
    print(f"\n{'='*70}")
    print("Translation metrics generation completed!")
    print(f"{'='*70}\n")


if __name__ == '__main__':
    main()
