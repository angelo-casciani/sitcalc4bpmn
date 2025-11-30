# BPMN Reasoning Performance Evaluation

This evaluation framework provides comprehensive performance analysis of BPMN model translation and reasoning tasks.

## Overview

The framework evaluates BPMN models across four reasoning tasks:
- **Legality**: Checking executability of action sequences
- **Conformance**: Verifying execution traces against process specifications
- **Projection**: Determining fluent truth values after action sequences
- **Property Verification**: Checking properties over complete executions

## Evaluation Directory Structure

```
evaluation/
├── datasets/                              # Benchmarks generated from the BPMN datasets
│   ├── ...
├── results/                               # Evaluation results
│   ├── ...
│   └── charts/                            # Generated charts summarizing the results 
│       ├── ...
├── bpmn_metrics/                          # Metrics from the BPMN models
│   ├── ...
└── src/
    ├── bpmn_metrics.py                    # Extracts and computes structural metrics for all BPMN models in both datasets
    ├── generate_translation_metrics.py    # Translates all BPMN models and collects translation performance metrics
    ├── evaluate.py                        # Orchestrates the evaluation process
    ├── chart_generator.py                 # Generates charts from evaluation results
    ├── sample_generator.py                # Generates test samples for legality and conformance tasks
    ├── trace_generator.py                 # Simulates BPMN execution to generate valid process traces with element types and gateway values
    ├── metrics_collector.py               # Extracts performance metrics from Prolog output
    ├── report_generator.py                # Collects metrics from the generated results
    └── bpmn_metrics.py                    # Core module for parsing BPMN XML files
```

## Usage

### Computing BPMN Structural Metrics (One-time Setup)

To compute structural metrics for all BPMN models:
```bash
python evaluation/src/bpmn_metrics.py --compute-all
```

This creates:
- `evaluation/bpmn_metrics/bpmn_metrics_processed.csv` (89 models for legality/conformance)
- `evaluation/bpmn_metrics/bpmn_metrics_exams.csv` (10 models for projection/verification)
- `evaluation/bpmn_metrics/bpmn_metrics_processed_summary.txt` (summary statistics)
- `evaluation/bpmn_metrics/bpmn_metrics_exams_summary.txt` (summary statistics)

**Note**: This step is independent of evaluation runs and only needs to be done once (or when BPMN models change).

### Generating Translation Metrics

To generate translation metrics with action and fluent counts:
```bash
python evaluation/src/generate_translation_metrics.py
```

This creates:
- `evaluation/bpmn_metrics/translation_metrics_leg_conf.csv` (89 models)
- `evaluation/bpmn_metrics/translation_metrics_exams.csv` (10 models)
- `evaluation/bpmn_metrics/translation_metrics_leg_conf_summary.txt` (summary statistics)
- `evaluation/bpmn_metrics/translation_metrics_exams_summary.txt` (summary statistics)

Each CSV contains:
- `translation_time_sec`: Time taken to translate the BPMN model
- `num_actions`: Number of `prim_action` declarations in generated Prolog
- `num_fluents`: Number of `rel_fluent` and `fun_fluent` declarations
- `program_size_lines`: Total lines of generated Prolog code

Summary files include:
- Overall statistics (total models, actions, fluents)
- Translation time statistics (mean, median, std dev, min, max)
- Actions and fluents statistics
- Program size statistics
- Top 5 models by translation time, complexity, and program size
- Correlation analysis between metrics

**Note**: This step can be run independently or will be automatically executed during evaluation.

### Running Evaluations (Test Mode)

Run both evaluations:
```bash
python evaluation/src/evaluate.py --mode test --task all
```

Run only legality & conformance:
```bash
python evaluation/src/evaluate.py --mode test --task legality_conformance
```

Run only projection & verification:
```bash
python evaluation/src/evaluate.py --mode test --task projection_verification
```
Translation metrics should be generated using `generate_translation_metrics.py`.

### Resume Incomplete Evaluation

Continue from an incomplete CSV file:
```bash
python evaluation/src/evaluate.py --mode test --task legality_conformance \
  --resume evaluation/results/evaluation_results_leg_conf_2025-11-10_09:44:50.csv
```

The resume feature:
- Loads existing results from the CSV
- Skips already evaluated samples
- Appends new results to the same file
- Preserves the original timestamp

### Generating Summaries and Charts (Assess Mode)

The assess mode generates both summary reports and visualization charts.

Generate summary and charts from the latest CSV:
```bash
python evaluation/src/evaluate.py --mode assess --task legality_conformance
```

Generate summary and charts from a specific CSV:
```bash
python evaluation/src/evaluate.py --mode assess --task projection_verification \
  --csv evaluation/results/evaluation_results_proj_verif_2025-11-10_09:44:50.csv
```

Assess both task types (generates summaries and charts for both):
```bash
python evaluation/src/evaluate.py --mode assess --task all
```

### Generating Charts Independently

You can also generate charts without running the full assessment:
```bash
# Generate charts for all CSV files in results directory
python evaluation/src/chart_generator.py

# Generate charts for a specific CSV file
python evaluation/src/chart_generator.py --csv-file evaluation/results/evaluation_results_proj_verif_2025-11-10_09:44:50.csv
```

## Metrics Collected

### BPMN Model Metrics (pre-computed with `compute_bpmn_metrics.py`)
Stored in `bpmn_metrics/bpmn_metrics_processed.csv` and `bpmn_metrics/bpmn_metrics_exams.csv`:
- Number of tasks
- Number of exclusive gateways
- Number of parallel gateways
- Number of events
- Number of pools
- Number of subprocesses
- Number of data objects
- Total elements

### Translation Metrics
Stored in `bpmn_metrics/translation_metrics_*.csv`:
- Translation time (seconds)
- Number of primitive actions
- Number of fluents
- Program size (lines of Prolog code)

### Evaluation Results (per sample)
- Model name
- Task type (legality, conformance, projection, verification)
- Sample ID
- Expected result (true/false)
- Returned result (true/false)
- Correctness (correct/incorrect)
- Reasoning time (seconds)
- Number of Prolog inferences
- Status (success, failure, conforms, not_conforms, error, or timeout)
- Error Message

### Summary Metrics (generated from CSV in assess mode)
- Total samples evaluated
- Overall accuracy, precision, recall, F1-score
- Accuracy by task type:
  - Legality
  - Conformance
  - Projection
  - Property Verification
- Average reasoning time per task type
- Average inferences per task type

### Visualization Charts (generated in assess mode)
Three charts per evaluation task, each showing two lines (one per task type):
1. **Accuracy Chart**: Percentage of correct predictions vs process model number
2. **Reasoning Time Chart**: Average reasoning time (seconds) vs process model number
3. **Inferences Chart**: Average number of inferences vs process model number

Notes on errors/timeouts:
- Rows with `status` == `error` or `timeout` are excluded from the accuracy/time/inference charts and per-task averages to avoid skewing the results. These counts are reported separately in the generated summary text.

Charts are saved as high-resolution PNG files (300 DPI) in `results/charts/`.

## Sample Generation Details

### Legality & Conformance Samples (8 per model)
Generated from simulated BPMN traces:
- **4 legality samples**: 2 small (25%), 2 large (50%) - half true, half false
- **4 conformance samples**: 2 small (25%), 2 large (50%) - half true, half false
- False samples created by swapping task order in traces

### Projection & Verification Samples
Manual samples in `datasets/samples_proj_verif.csv` for process models in `bpmn/exams-bpmn/`:
- Control flow fluents (done predicates)
- Data object fluents
- Temporal properties
- Process completion properties

## Command Reference

### Test Mode Arguments
- `--mode test`: Run evaluations
- `--task {legality_conformance,projection_verification,all}`: Which evaluation to run
- `--resume <path>`: Resume from incomplete CSV file

### Assess Mode Arguments
- `--mode assess`: Generate summaries and charts from CSV
- `--task {legality_conformance,projection_verification,all}`: Which CSV to assess
- `--csv <path>`: Specific CSV file to assess (optional, finds latest if not provided)

**Note**: Assess mode automatically generates both text summaries and visualization charts.

## Testing Individual Components

### Test BPMN Metrics Extraction
```bash
python evaluation/src/bpmn_metrics.py bpmn/dataset/processed/process_1.bpmn
```

### Test Sample Generation
```bash
python evaluation/src/sample_generator.py
```

Generates `evaluation/datasets/samples_leg_conf.csv` from all models in `bpmn/dataset/processed/`.

### Test Trace Generation
```bash
python evaluation/src/trace_generator.py bpmn/dataset/processed/process_1.bpmn
```

### Test Chart Generation
```bash
# Generate charts for all CSV files
python evaluation/src/chart_generator.py

# Generate charts for a specific CSV file
python evaluation/src/chart_generator.py --csv-file evaluation/results/evaluation_results_leg_conf_2025-11-10_14:50:10.csv
```

## Notes

- The evaluation framework integrates with existing `translator_service.py` and `reasoning_service.py` modules
- Sample generation uses trace simulation to create realistic test cases
- BPMN metrics are extracted automatically during evaluation and saved separately
- Summary reports and charts are generated from CSV files, allowing re-assessment without re-running evaluations
- Resume functionality prevents data loss from interrupted evaluations
- Charts are automatically generated in assess mode for visual analysis of results
- Process model numbers are extracted from model names (e.g., `process_10.bpmn` → 10) for chart x-axis
- All metrics are aggregated and averaged per model and task type for chart generation

## Workflow Example

1. **Generate samples** (if not already done):
   ```bash
   python evaluation/src/sample_generator.py
   ```

2. **Run evaluation**:
   ```bash
   python evaluation/src/evaluate.py --mode test --task legality_conformance
   ```

3. **If interrupted, resume**:
   ```bash
   python evaluation/src/evaluate.py --mode test --task legality_conformance \
     --resume evaluation/results/evaluation_results_leg_conf_2025-11-10_09:44:50.csv
   ```

4. **Generate or regenerate summary and charts**:
   ```bash
   python evaluation/src/evaluate.py --mode assess --task legality_conformance
   ```
   This creates:
   - Summary text file in `results/`
   - Three chart PNG files in `results/charts/`