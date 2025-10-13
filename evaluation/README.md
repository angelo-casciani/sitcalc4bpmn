# BPMN Reasoning Performance Evaluation

This evaluation framework provides comprehensive performance analysis of BPMN model translation and reasoning tasks.

## Overview

The framework evaluates BPMN models across four reasoning tasks:
- **Projection**: Determining fluent truth values after action sequences
- **Legality**: Checking executability of action sequences
- **Conformance**: Verifying execution traces against process specifications
- **Property Verification**: Checking temporal properties over complete executions

## Components

### 1. BPMN Metrics Extractor (`bpmn_metrics.py`)
Parses BPMN XML files and extracts:
- Structural metrics: #tasks, #exclusive gateways, #parallel gateways, #events, #pools, #subprocesses
- Elements for sample generation: task names, gateway conditions, data objects

### 2. Sample Generator (`sample_generator.py`)
Generates test samples for each reasoning task:
- 6 projection samples (3 true, 3 false) covering control flow, gateway conditions, and data objects
- 6 legality samples (mix of legal/illegal action sequences)
- 6 conformance samples (mix of conforming/non-conforming traces)
- 6 property verification samples (1-2 properties per sample)

**Total: 24 samples per BPMN model**

### 3. Metrics Collector (`metrics_collector.py`)
Extracts performance metrics:
- **Reasoning metrics**: time, CPU time, inferences (from Prolog output)
- **Translation metrics**: time, memory usage, #actions, #fluents, program size
- **Accuracy metrics**: accuracy, precision, recall, F1, MCC, AUC

### 4. Main Evaluation Script (`evaluate.py`)
Orchestrates the evaluation process:
1. Discovers BPMN models in `bpmn/dataset/processed/`
2. Translates each model to IndiGolog
3. Generates test samples
4. Executes reasoning tasks
5. Collects metrics
6. Computes accuracy metrics
7. Saves results to CSV and TXT

### 5. Report Generator (`report_generator.py`)
Creates visualizations and reports:
- **Plots**:
  - Reasoning time vs model size
  - Task completion time per reasoning type
  - Accuracy by task type
  - Inference distribution
  - Translation metrics (time and memory)
- **Tables**: Statistics summary

## Usage

### Basic Evaluation

Evaluate all BPMN models in the dataset:

```bash
cd src
python evaluate.py
```

### Evaluate Specific Models

Filter models by name:

```bash
python evaluate.py --models E_j01 E_j02
```

### Limit Number of Models

Evaluate only the first N models:

```bash
python evaluate.py --limit 5
```

### Custom Paths

Specify custom dataset and output directories:

```bash
python evaluate.py --dataset-dir /path/to/dataset --output-dir /path/to/output
```

### Generate Reports

After running evaluation, generate visualizations:

```bash
python report_generator.py
```

Or specify custom paths:

```bash
python report_generator.py --results-csv /path/to/results.csv --output-dir /path/to/output
```

## Output Structure

```
evaluation/
├── datasets/
│   ├── model1_samples.csv
│   ├── model2_samples.csv
│   └── ...
├── results/
│   ├── detailed_results.csv
│   ├── summary_report.txt
│   └── statistics_table.txt
└── plots/
    ├── reasoning_time_vs_model_size.png
    ├── task_completion_time.png
    ├── accuracy_by_task_type.png
    ├── inference_distribution.png
    └── translation_metrics.png
```

## Metrics Collected

### BPMN Model Metrics
- Number of tasks
- Number of exclusive gateways
- Number of parallel gateways
- Number of events
- Number of pools
- Number of subprocesses

### Translation Metrics
- Translation time (seconds)
- Memory usage (MB)
- Number of actions in generated program
- Number of fluents in generated program
- Program size (lines of code)

### Reasoning Metrics (per sample)
- Success/failure status
- Reasoning time (seconds)
- CPU time (seconds)
- Number of Prolog inferences
- Result (true/false/success/failure/conforms/not_conforms)

### Accuracy Metrics (per task type)
- Accuracy: (TP + TN) / Total
- Precision: TP / (TP + FP)
- Recall: TP / (TP + FN)
- F1 Score: Harmonic mean of precision and recall
- MCC: Matthews Correlation Coefficient
- AUC: Area Under ROC Curve (approximation)

## Sample Generation Details

### Projection Samples
- **Sample 1-2**: Control flow fluents (done predicates)
  - 1 true: Task is done after completion
  - 1 false: Task not done initially
- **Sample 3-4**: Gateway condition fluents
  - 1 true: Condition becomes true after action
  - 1 false: Condition remains false initially
- **Sample 5-6**: Data object fluents
  - 1 true: Data object created after task
  - 1 false: Data object doesn't exist initially

### Legality Samples
- 3 legal sequences (empty, single task, sequential tasks)
- 3 illegal sequences (end before start, double start, missing prerequisites)

### Conformance Samples
- 3 conforming traces (proper execution order)
- 3 non-conforming traces (wrong order, skipped tasks, incomplete execution)

### Property Verification Samples
Properties tested:
- Eventually task completes
- Data object is created
- Multiple tasks complete
- Mutual exclusion properties
- Process can complete
- Impossible state detection

## Testing Individual Components

### Test BPMN Metrics Extraction
```bash
python bpmn_metrics.py ../bpmn/dataset/processed/E_j01/0.bpmn2.xml
```

### Test Sample Generation
```bash
python sample_generator.py ../bpmn/dataset/processed/E_j01/0.bpmn2.xml
```

### Test Metrics Collector
```bash
python metrics_collector.py
```

## Requirements

- Python 3.8+
- SWI-Prolog
- Required packages (install with `pip install -r requirements.txt`):
  - gradio>=4.0.0
  - pm4py>=2.7.0
  - pillow>=10.0.0
  - matplotlib>=3.5.0
  - numpy>=1.21.0

## Notes

- The evaluation framework integrates with existing `bpmn2indi_cli.py` and `reason.py` modules
- Expected results are determined based on BPMN semantics, not the generated translation
- All metrics are extracted directly from Prolog output when available
- Memory usage is tracked during translation only
- The framework handles errors gracefully and continues evaluation even if individual models fail

## Example Output

### Summary Report (excerpt)
```
======================================================================
BPMN REASONING EVALUATION - SUMMARY REPORT
======================================================================

Total models evaluated: 12

----------------------------------------------------------------------
Model: E_j01_0
----------------------------------------------------------------------
BPMN Metrics:
  Tasks: 8
  Exclusive Gateways: 2
  Parallel Gateways: 0
  Events: 3

Translation Metrics:
  Time: 0.234 sec
  Memory: 12.45 MB
  Actions: 24
  Fluents: 18

Accuracy by Task Type:
  projection:
    Accuracy:  0.833
    Precision: 0.800
    Recall:    1.000
    F1 Score:  0.889
    MCC:       0.745
```

## Troubleshooting

### "Model file not found" error
Ensure the model has been translated first. The evaluation script automatically translates models.

### "swipl not found" error
Install SWI-Prolog: `sudo apt-get install swi-prolog`

### Memory issues
Reduce the number of models evaluated at once using `--limit`.

### Timeout errors
Some complex models may take longer to reason. Adjust timeout in `reason.py` if needed.

## Future Enhancements

- Parallel execution for faster evaluation
- More sophisticated property generation
- Interactive visualization dashboard
- Comparison with other reasoning engines
- Extended temporal property templates
