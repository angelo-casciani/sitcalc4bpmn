"""
Metrics Collector for BPMN Evaluation

This module extracts performance metrics from:
- Prolog reasoning output (time, CPU, inferences)
- Translation process (time, memory, #actions, #fluents)
"""

import re
import os
import time
import tracemalloc
from dataclasses import dataclass
from typing import Optional, Tuple


@dataclass
class ReasoningMetrics:
    """Container for reasoning task performance metrics."""
    success: bool
    reasoning_time: float  # seconds
    cpu_time: float  # seconds
    inferences: int
    result: str  # "true", "false", "success", "failure", "conforms", "not_conforms"
    
    def to_dict(self):
        """Convert to dictionary for CSV export."""
        return {
            'success': self.success,
            'reasoning_time_sec': self.reasoning_time,
            'cpu_time_sec': self.cpu_time,
            'inferences': self.inferences,
            'result': self.result
        }


@dataclass
class TranslationMetrics:
    """Container for translation performance metrics."""
    translation_time: float  # seconds
    memory_usage: float  # MB
    num_actions: int
    num_fluents: int
    program_size_lines: int
    
    def to_dict(self):
        """Convert to dictionary for CSV export."""
        return {
            'translation_time_sec': self.translation_time,
            'memory_usage_mb': self.memory_usage,
            'num_actions': self.num_actions,
            'num_fluents': self.num_fluents,
            'program_size_lines': self.program_size_lines
        }


class MetricsCollector:
    """Collect and parse performance metrics from various sources."""
    
    @staticmethod
    def parse_prolog_output(output: str, success: bool) -> ReasoningMetrics:
        """Parse Prolog output to extract performance metrics.
        
        Args:
            output: Raw Prolog output text
            success: Whether the reasoning task succeeded
            
        Returns:
            ReasoningMetrics object with extracted metrics
        """
        # Default values
        reasoning_time = 0.0
        cpu_time = 0.0
        inferences = 0
        result = "unknown"
        
        print(f"        [DEBUG METRICS] Parsing output (length: {len(output)} chars)")
        print(f"        [DEBUG METRICS] Success flag: {success}")
        
        # Pattern for inference and time: "X inferences, Y CPU in Z seconds"
        # Example: "21,914,474 inferences, 0.812 CPU in 0.813 seconds"
        inference_pattern = r'([\d,]+)\s+inferences?,\s+([\d.]+)\s+CPU\s+in\s+([\d.]+)\s+seconds?'
        match = re.search(inference_pattern, output)
        
        if match:
            inferences_str = match.group(1).replace(',', '')
            inferences = int(inferences_str)
            cpu_time = float(match.group(2))
            reasoning_time = float(match.group(3))
            print(f"        [DEBUG METRICS] Found timing info: {inferences} inferences, {reasoning_time}s")
        else:
            print(f"        [DEBUG METRICS] No timing info found in output")
        
        # Determine result from output
        print(f"        [DEBUG METRICS] Checking result markers in output:")
        print(f"                        'RESULT: SUCCESS' found: {'RESULT: SUCCESS' in output}")
        print(f"                        'RESULT: FAILURE' found: {'RESULT: FAILURE' in output}")
        print(f"                        'RESULT: CONFORMANT' found: {'RESULT: CONFORMANT' in output}")
        print(f"                        'RESULT: NON-CONFORMANT' found: {'RESULT: NON-CONFORMANT' in output}")
        print(f"                        '✓' markers found: {'✓' in output}")
        print(f"                        '✗' markers found: {'✗' in output}")
        
        # Check for IndiGolog execution status (legality checks)
        if "Program has executed to completion" in output:
            result = "success"
            print(f"        [DEBUG METRICS] Result set to: success (IndiGolog completed)")
        
        elif "PROGRAM: Program fails" in output or "Program fails:" in output:
            result = "failure"
            print(f"        [DEBUG METRICS] Result set to: failure (IndiGolog program failure)")
        
        # Check for success markers (projection, property verification, legality)
        elif ("RESULT: SUCCESS" in output or 
              "RESULT: Action sequence is EXECUTABLE" in output or
              "✓ Query evaluation: TRUE" in output or
              "✓ Action sequence is EXECUTABLE" in output or
              "✓ Property can be satisfied" in output):
            result = "success"
            print(f"        [DEBUG METRICS] Result set to: success")
        
        # Check for failure markers (projection, property verification, legality)
        elif ("RESULT: FAILURE" in output or 
              "RESULT: Action sequence is NOT EXECUTABLE" in output or
              "program fails" in output.lower() or
              "✗ Query evaluation: FALSE" in output or
              "✗ Action sequence is NOT EXECUTABLE" in output or
              "✗ Property cannot be satisfied" in output):
            result = "failure"
            print(f"        [DEBUG METRICS] Result set to: failure")
        
        # Check for conformance markers
        elif ("RESULT: CONFORMANT" in output or 
              "✓ History is CONFORMANT" in output):
            result = "conforms"
            print(f"        [DEBUG METRICS] Result set to: conforms")
        
        elif ("RESULT: NON-CONFORMANT" in output or 
              "✗ History is NON-CONFORMANT" in output):
            result = "not_conforms"
            print(f"        [DEBUG METRICS] Result set to: not_conforms")
        
        # Check for error cases
        elif "RESULT: ERROR" in output or ("ERROR:" in output and "RESULT:" not in output):
            result = "error"
            print(f"        [DEBUG METRICS] Result set to: error")
        
        else:
            print(f"        [DEBUG METRICS] Result remains: unknown (no markers found)")
            print(f"        [DEBUG METRICS] Output preview: {output[:500]}")
        
        return ReasoningMetrics(
            success=success,
            reasoning_time=reasoning_time,
            cpu_time=cpu_time,
            inferences=inferences,
            result=result
        )
    
    @staticmethod
    def measure_translation(bpmn_file_path: str, output_pl_path: str,
                          translate_func) -> TranslationMetrics:
        """Measure translation performance metrics.
        
        Args:
            bpmn_file_path: Path to input BPMN file
            output_pl_path: Path where translated Prolog file will be written
            translate_func: Function that performs the translation
            
        Returns:
            TranslationMetrics object
        """
        # Start memory tracking
        tracemalloc.start()
        
        # Measure translation time
        start_time = time.time()
        
        try:
            # Execute translation
            translate_func()
            
            translation_time = time.time() - start_time
            
            # Get memory usage
            current, peak = tracemalloc.get_traced_memory()
            memory_usage_mb = peak / (1024 * 1024)  # Convert to MB
            
        finally:
            tracemalloc.stop()
        
        # Count actions and fluents in generated file
        num_actions, num_fluents, program_lines = MetricsCollector._analyze_prolog_program(
            output_pl_path
        )
        
        return TranslationMetrics(
            translation_time=translation_time,
            memory_usage=memory_usage_mb,
            num_actions=num_actions,
            num_fluents=num_fluents,
            program_size_lines=program_lines
        )
    
    @staticmethod
    def _analyze_prolog_program(prolog_file_path: str) -> Tuple[int, int, int]:
        """Analyze generated Prolog program to count actions and fluents.
        
        Args:
            prolog_file_path: Path to the Prolog file
            
        Returns:
            Tuple of (num_actions, num_fluents, num_lines)
        """
        if not os.path.exists(prolog_file_path):
            return 0, 0, 0
        
        num_actions = 0
        num_fluents = 0
        num_lines = 0
        
        action_pattern = re.compile(r'^\s*prim_action\(')
        fluent_pattern = re.compile(r'^\s*(rel_fluent|fun_fluent)\(')
        
        try:
            with open(prolog_file_path, 'r') as f:
                for line in f:
                    num_lines += 1
                    line = line.strip()
                    
                    # Skip comments and empty lines
                    if not line or line.startswith('%'):
                        continue
                    
                    # Count action declarations
                    if action_pattern.match(line):
                        num_actions += 1
                    
                    # Count fluent declarations
                    if fluent_pattern.match(line):
                        num_fluents += 1
        
        except Exception as e:
            print(f"Warning: Error analyzing Prolog file {prolog_file_path}: {e}")
        
        return num_actions, num_fluents, num_lines
    
    @staticmethod
    def compute_accuracy_metrics(predictions: list, ground_truth: list) -> dict:
        """Compute accuracy metrics for binary classification.
        
        Args:
            predictions: List of predicted values (True/False)
            ground_truth: List of expected values (True/False)
            
        Returns:
            Dictionary with accuracy, precision, recall, F1, MCC, and AUC
        """
        if len(predictions) != len(ground_truth):
            raise ValueError("Predictions and ground truth must have same length")
        
        if len(predictions) == 0:
            return {
                'accuracy': 0.0,
                'precision': 0.0,
                'recall': 0.0,
                'f1_score': 0.0,
                'mcc': 0.0,
                'auc': 0.0
            }
        
        # Convert to binary (1 = success/true/conforms, 0 = failure/false/not_conforms)
        pred_binary = [1 if p else 0 for p in predictions]
        true_binary = [1 if t else 0 for t in ground_truth]
        
        # Calculate confusion matrix values
        tp = sum(1 for p, t in zip(pred_binary, true_binary) if p == 1 and t == 1)
        tn = sum(1 for p, t in zip(pred_binary, true_binary) if p == 0 and t == 0)
        fp = sum(1 for p, t in zip(pred_binary, true_binary) if p == 1 and t == 0)
        fn = sum(1 for p, t in zip(pred_binary, true_binary) if p == 0 and t == 1)
        
        total = tp + tn + fp + fn
        
        # Accuracy
        accuracy = (tp + tn) / total if total > 0 else 0.0
        
        # Precision
        precision = tp / (tp + fp) if (tp + fp) > 0 else 0.0
        
        # Recall (Sensitivity)
        recall = tp / (tp + fn) if (tp + fn) > 0 else 0.0
        
        # F1 Score
        f1_score = (2 * precision * recall) / (precision + recall) if (precision + recall) > 0 else 0.0
        
        # Matthews Correlation Coefficient (MCC)
        mcc_numerator = (tp * tn) - (fp * fn)
        mcc_denominator = ((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn)) ** 0.5
        mcc = mcc_numerator / mcc_denominator if mcc_denominator > 0 else 0.0
        
        # AUC (simple approximation using sensitivity and specificity)
        specificity = tn / (tn + fp) if (tn + fp) > 0 else 0.0
        auc = (recall + specificity) / 2.0
        
        return {
            'accuracy': accuracy,
            'precision': precision,
            'recall': recall,
            'f1_score': f1_score,
            'mcc': mcc,
            'auc': auc,
            'tp': tp,
            'tn': tn,
            'fp': fp,
            'fn': fn
        }
    
    @staticmethod
    def extract_result_from_output(output: str, expected_type: str) -> bool:
        """Extract boolean result from reasoning output.
        
        Args:
            output: Raw output text
            expected_type: Type of expected result ("legality", "conformance")
            
        Returns:
            Boolean indicating success/true/conforms
        """
        output_lower = output.lower()
    
        if expected_type == "legality":
            return "result: success" in output_lower or "legal" in output_lower
        elif expected_type == "conformance":
            return "result: conformant" in output_lower or "conforms" in output_lower
        
        # Default: check for general success indicators
        return "success" in output_lower or "true" in output_lower


if __name__ == '__main__':
    # Test accuracy metrics computation
    print("Testing Accuracy Metrics Computation")
    print("=" * 70)
    
    # Example: Perfect predictions
    predictions = [True, True, False, False, True, False]
    ground_truth = [True, True, False, False, True, False]
    
    metrics = MetricsCollector.compute_accuracy_metrics(predictions, ground_truth)
    print("\nPerfect predictions:")
    for key, value in metrics.items():
        if isinstance(value, float):
            print(f"  {key}: {value:.4f}")
        else:
            print(f"  {key}: {value}")
    
    # Example: Some errors
    predictions2 = [True, True, False, True, True, False]
    ground_truth2 = [True, True, False, False, True, False]
    
    metrics2 = MetricsCollector.compute_accuracy_metrics(predictions2, ground_truth2)
    print("\nWith one false positive:")
    for key, value in metrics2.items():
        if isinstance(value, float):
            print(f"  {key}: {value:.4f}")
        else:
            print(f"  {key}: {value}")
