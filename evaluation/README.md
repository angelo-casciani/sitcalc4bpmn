# BPMN Reasoning Performance Evaluation

This evaluation framework provides comprehensive performance analysis of BPMN model translation and reasoning tasks.

## Overview

The framework evaluates BPMN models across four reasoning tasks:
- **Legality**: Checking executability of action sequences
- **Conformance**: Verifying execution traces against process specifications
- **Projection**: Determining fluent truth values after action sequences
- **Property Verification**: Checking temporal properties over complete executions

## Components

### 1. BPMN Metrics Extractor (`bpmn_metrics.py`)
Parses BPMN XML files and extracts:
- Structural metrics: #tasks, #exclusive gateways, #parallel gateways, #events, #pools, #subprocesses, #data objects
- Elements for sample generation: task names, gateway conditions, data objects

### 2. Sample Generator (`sample_generator.py`)
Generates test samples for legality and conformance tasks:
- **8 samples per model** (4 legality + 4 conformance)
  - 2 small (25% of trace) + 2 large (50% of trace) per task
  - 1 true + 1 false for each size category
- Outputs: `datasets/samples_leg_conf.csv`

### 3. Trace Generator (`trace_generator.py`)
Simulates BPMN execution to generate valid process traces with element types and gateway values.

### 4. Metrics Collector (`metrics_collector.py`)
Extracts performance metrics from Prolog output:
- Reasoning time, inferences, success/failure status

### 5. Main Evaluation Script (`evaluate.py`)
Orchestrates the evaluation process with two modes:
- **Test mode**: Run evaluations and collect results
- **Assess mode**: Generate summaries from existing CSV files

## Usage

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

### Generating Summaries (Assess Mode)

Generate summary from the latest CSV:
```bash
python evaluation/src/evaluate.py --mode assess --task legality_conformance
```

Generate summary from a specific CSV:
```bash
python evaluation/src/evaluate.py --mode assess --task projection_verification \
  --csv evaluation/results/evaluation_results_proj_verif_2025-11-10_09:44:50.csv
```

Assess both task types:
```bash
python evaluation/src/evaluate.py --mode assess --task all
```

## Output Structure

```
evaluation/
├── datasets/
│   ├── samples_leg_conf.csv          # Legality & conformance samples
│   └── samples_proj_verif.csv        # Projection & verification samples
├── results/
│   ├── evaluation_results_leg_conf_YYYY-MM-DD_HH:MM:SS.csv
│   ├── evaluation_summary_leg_conf_YYYY-MM-DD_HH:MM:SS.txt
│   ├── evaluation_results_proj_verif_YYYY-MM-DD_HH:MM:SS.csv
│   └── evaluation_summary_proj_verif_YYYY-MM-DD_HH:MM:SS.txt
└── bpmn_metrics/
    ├── bpmn_metrics_leg_conf.csv
    └── bpmn_metrics_proj_verif.csv
```

## Metrics Collected

### BPMN Model Metrics (extracted during evaluation)
- Number of tasks
- Number of exclusive gateways
- Number of parallel gateways
- Number of events
- Number of pools
- Number of subprocesses
- Number of data objects
- Total elements

### Evaluation Results (per sample)
- Model name
- Task type (legality, conformance, projection, verification)
- Sample ID
- Expected result (true/false)
- Returned result (true/false)
- Correctness (correct/incorrect)
- Reasoning time (seconds)
- Number of Prolog inferences

### Summary Metrics (generated from CSV)
- Total samples evaluated
- Overall accuracy
- Accuracy by task type:
  - Legality
  - Conformance
  - Projection
  - Property Verification

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
- `--mode assess`: Generate summaries from CSV
- `--task {legality_conformance,projection_verification,all}`: Which CSV to assess
- `--csv <path>`: Specific CSV file to assess (optional, finds latest if not provided)

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

## Requirements

- Python 3.8+
- SWI-Prolog
- Required packages (install with `pip install -r ../requirements.txt`)

## Notes

- The evaluation framework integrates with existing `translator_service.py` and `reasoning_service.py` modules
- Sample generation uses trace simulation to create realistic test cases
- BPMN metrics are extracted automatically during evaluation and saved separately
- Summary reports are generated from CSV files, allowing re-assessment without re-running evaluations
- Resume functionality prevents data loss from interrupted evaluations

## Example Output

### Summary Report (excerpt)
```
======================================================================
EVALUATION SUMMARY
======================================================================
Total samples: 80
Correct: 72
Overall Accuracy: 90.0%

Legality: 38/40 (95.0%)
Conformance: 34/40 (85.0%)
======================================================================
```

### CSV Output (evaluation_results_leg_conf_*.csv)
```
model_name,task_type,sample_id,expected,returned,correct,reasoning_time,inferences
process_1.bpmn,legality,1,True,True,True,0.234,1523
process_1.bpmn,legality,2,False,False,True,0.189,1201
...
```

### BPMN Metrics CSV (bpmn_metrics_*.csv)
```
model_name,num_tasks,num_exclusive_gateways,num_parallel_gateways,num_events,num_pools,num_subprocesses,num_data_objects,total_elements
process_1.bpmn,8,2,1,3,1,0,2,15
...
```

## Troubleshooting

### "Model file not found" error
Ensure BPMN files are in `bpmn/dataset/processed/` for legality/conformance or `bpmn/exams-bpmn/` for projection/verification.

### "swipl not found" error
Install SWI-Prolog: `sudo apt-get install swi-prolog`

### Resume not working
Ensure the CSV file path is correct and the file exists. The timestamp is extracted from the filename.

### Missing samples in CSV
Run sample generation first: `python evaluation/src/sample_generator.py`

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

4. **Generate or regenerate summary**:
   ```bash
   python evaluation/src/evaluate.py --mode assess --task legality_conformance
   ```