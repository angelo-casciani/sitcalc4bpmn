#!/bin/bash
# Complete BPMN Dataset Evaluation Script
# This script runs a full evaluation on all BPMN models in the dataset

set -e  # Exit on error

echo "=========================================================================="
echo "BPMN REASONING EVALUATION - COMPLETE DATASET"
echo "=========================================================================="
echo ""
echo "This script will:"
echo "  1. Evaluate ALL BPMN models in bpmn/dataset/processed/"
echo "  2. Generate 24 test samples per model"
echo "  3. Run reasoning tasks (projection, legality, conformance, property)"
echo "  4. Collect performance metrics"
echo "  5. Generate comprehensive reports and visualizations"
echo ""
echo "⚠️  Warning: This may take a considerable amount of time depending on"
echo "   the number of models and their complexity."
echo ""

# Check if we're in the right directory
if [ ! -f "evaluate.py" ]; then
    echo "Error: Please run this script from the evaluation/src/ directory"
    echo "Usage: cd evaluation/src && bash run_complete_evaluation.sh"
    exit 1
fi

# Count number of BPMN models
MODEL_COUNT=$(find ../../bpmn/dataset/processed/ -name "*.bpmn2.xml" -o -name "*.bpmn" -o -name "*.xml" | grep -v ".txt" | wc -l)
echo "Found $MODEL_COUNT BPMN models to evaluate"
echo ""

read -p "Do you want to proceed? (y/n) " -n 1 -r
echo
if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    echo "Evaluation cancelled."
    exit 0
fi

echo ""
echo "=========================================================================="
echo "STEP 1: Running Complete Evaluation"
echo "=========================================================================="
echo ""

START_TIME=$(date +%s)

python evaluate.py

if [ $? -ne 0 ]; then
    echo ""
    echo "❌ Error: Evaluation failed. Please check the error messages above."
    exit 1
fi

END_TIME=$(date +%s)
DURATION=$((END_TIME - START_TIME))
MINUTES=$((DURATION / 60))
SECONDS=$((DURATION % 60))

echo ""
echo "✓ Evaluation completed in ${MINUTES}m ${SECONDS}s"
echo ""

echo "=========================================================================="
echo "STEP 2: Generating Reports and Visualizations"
echo "=========================================================================="
echo ""

python report_generator.py

if [ $? -ne 0 ]; then
    echo ""
    echo "❌ Error: Report generation failed. Please check the error messages above."
    exit 1
fi

echo ""
echo "=========================================================================="
echo "✓ COMPLETE EVALUATION FINISHED SUCCESSFULLY!"
echo "=========================================================================="
echo ""
echo "Results Location:"
echo "  📁 evaluation/"
echo "     ├── datasets/          Test samples (CSV)"
echo "     ├── results/           Evaluation metrics (CSV + TXT)"
echo "     │   ├── detailed_results.csv"
echo "     │   ├── summary_report.txt"
echo "     │   └── statistics_table.txt"
echo "     └── plots/             Visualizations (PNG)"
echo "         ├── reasoning_time_vs_model_size.png"
echo "         ├── task_completion_time.png"
echo "         ├── accuracy_by_task_type.png"
echo "         ├── inference_distribution.png"
echo "         └── translation_metrics.png"
echo ""
echo "Total evaluation time: ${MINUTES}m ${SECONDS}s"
echo ""
echo "Next Steps:"
echo "  • View summary report:     cat ../results/summary_report.txt"
echo "  • Open plots:              xdg-open ../plots/"
echo "  • Analyze detailed data:   libreoffice ../results/detailed_results.csv"
echo ""
