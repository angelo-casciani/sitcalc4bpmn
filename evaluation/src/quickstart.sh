#!/bin/bash
# Quick Start Guide for BPMN Performance Evaluation

echo "=================================================="
echo "BPMN Performance Evaluation - Quick Start Guide"
echo "=================================================="
echo ""

# Check if we're in the right directory
if [ ! -f "evaluate.py" ]; then
    echo "Error: Please run this script from the evaluation/src/ directory"
    echo "Usage: cd evaluation/src && bash quickstart.sh"
    exit 1
fi

echo "Step 1: Testing the framework..."
python test_evaluation.py
if [ $? -ne 0 ]; then
    echo "Error: Framework test failed. Please check the installation."
    exit 1
fi

echo ""
echo "Step 2: Running evaluation on first 2 models (for testing)..."
echo "This will take a few minutes..."
echo ""
python evaluate.py --limit 2

if [ $? -eq 0 ]; then
    echo ""
    echo "Step 3: Generating visualizations..."
    python report_generator.py
    
    echo ""
    echo "=================================================="
    echo "✓ Evaluation complete!"
    echo "=================================================="
    echo ""
    echo "Results saved to:"
    echo "  - ../datasets/        (test samples)"
    echo "  - ../results/         (CSV and TXT reports)"
    echo "  - ../plots/           (visualizations)"
    echo ""
    echo "To run full evaluation on all models:"
    echo "  python evaluate.py"
    echo ""
    echo "To run on specific models:"
    echo "  python evaluate.py --models E_j01 E_j02"
    echo ""
else
    echo ""
    echo "Error: Evaluation failed. Please check the error messages above."
    exit 1
fi
