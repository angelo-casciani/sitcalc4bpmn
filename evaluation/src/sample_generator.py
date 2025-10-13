"""
Sample Generator for BPMN Evaluation

This module generates test samples for different reasoning tasks:
- Projection: 6 samples (3 true, 3 false) with fluents from control flow, gateways, data objects
- Legality: 6 samples (mix of legal/illegal action sequences)
- Conformance: 6 samples (mix of conforming/non-conforming traces)
- Property Verification: 6 samples (1-2 properties per sample)
"""

import re
from dataclasses import dataclass
from typing import List, Tuple, Optional
from enum import Enum
from bpmn_metrics import BPMNMetrics


class ReasoningTask(Enum):
    """Types of reasoning tasks."""
    PROJECTION = "projection"
    LEGALITY = "legality"
    CONFORMANCE = "conformance"
    PROPERTY_VERIFICATION = "property_verification"


class ExpectedResult(Enum):
    """Expected results for reasoning tasks."""
    TRUE = "true"
    FALSE = "false"
    SUCCESS = "success"
    FAILURE = "failure"
    CONFORMS = "conforms"
    NOT_CONFORMS = "not_conforms"


@dataclass
class ReasoningSample:
    """Container for a single reasoning test sample."""
    task_type: ReasoningTask
    sample_id: int
    actions: List[str]
    fluent: Optional[str]
    property_expr: Optional[str]
    expected_result: ExpectedResult
    description: str
    
    def to_dict(self):
        """Convert to dictionary for CSV export."""
        return {
            'task_type': self.task_type.value,
            'sample_id': self.sample_id,
            'actions': ','.join(self.actions) if self.actions else '',
            'fluent': self.fluent or '',
            'property_expr': self.property_expr or '',
            'expected_result': self.expected_result.value,
            'description': self.description
        }


class SampleGenerator:
    """Generate test samples for BPMN reasoning evaluation."""
    
    def __init__(self, model_name: str, bpmn_metrics: BPMNMetrics):
        """Initialize the sample generator.
        
        Args:
            model_name: Name of the BPMN model
            bpmn_metrics: Extracted BPMN metrics
        """
        self.model_name = model_name
        self.metrics = bpmn_metrics
        self.id_param = "1"  # Default ID parameter for actions
    
    def generate_all_samples(self) -> List[ReasoningSample]:
        """Generate all 24 samples (6 per task type).
        
        Returns:
            List of all generated samples
        """
        samples = []
        samples.extend(self.generate_projection_samples())
        samples.extend(self.generate_legality_samples())
        samples.extend(self.generate_conformance_samples())
        samples.extend(self.generate_property_verification_samples())
        return samples
    
    def generate_projection_samples(self) -> List[ReasoningSample]:
        """Generate 6 projection samples (3 true, 3 false).
        
        Samples distributed across:
        - 2 control flow fluents (done)
        - 2 gateway condition fluents
        - 2 data object fluents
        
        Returns:
            List of projection samples
        """
        samples = []
        
        # Get task names for control flow fluents
        task_names = self._get_sanitized_task_names()
        gateway_conditions = self._get_sanitized_gateway_conditions()
        data_objects = self._get_sanitized_data_objects()
        
        # Sample 1: Control flow - TRUE (task is done after completing it)
        if len(task_names) >= 1:
            task = task_names[0]
            actions = [f"{task}(start, {self.id_param})", f"{task}(end, {self.id_param})"]
            samples.append(ReasoningSample(
                task_type=ReasoningTask.PROJECTION,
                sample_id=1,
                actions=actions,
                fluent=f"done({task}(end, {self.id_param}))",
                property_expr=None,
                expected_result=ExpectedResult.TRUE,
                description=f"Control flow: Task '{task}' should be done after completion"
            ))
        
        # Sample 2: Control flow - FALSE (task not done before starting it)
        if len(task_names) >= 2:
            task = task_names[1] if len(task_names) > 1 else task_names[0]
            actions = []  # Empty history
            samples.append(ReasoningSample(
                task_type=ReasoningTask.PROJECTION,
                sample_id=2,
                actions=actions,
                fluent=f"done({task}(end, {self.id_param}))",
                property_expr=None,
                expected_result=ExpectedResult.FALSE,
                description=f"Control flow: Task '{task}' should not be done initially"
            ))
        
        # Sample 3: Another control flow - TRUE (different task is done)
        if len(task_names) >= 2:
            task = task_names[1]
            actions = [f"{task}(start, {self.id_param})", f"{task}(end, {self.id_param})"]
            samples.append(ReasoningSample(
                task_type=ReasoningTask.PROJECTION,
                sample_id=3,
                actions=actions,
                fluent=f"done({task}(end, {self.id_param}))",
                property_expr=None,
                expected_result=ExpectedResult.TRUE,
                description=f"Control flow: Task '{task}' should be done after completion"
            ))
        
        # Sample 4: Gateway condition - FALSE (condition remains false)
        if len(gateway_conditions) >= 1:
            condition = gateway_conditions[0]
            actions = []
            samples.append(ReasoningSample(
                task_type=ReasoningTask.PROJECTION,
                sample_id=4,
                actions=actions,
                fluent=f"{condition}({self.id_param})",
                property_expr=None,
                expected_result=ExpectedResult.FALSE,
                description=f"Gateway condition: '{condition}' should be false initially"
            ))
        
        # Sample 5: Data object - TRUE (data object created)
        if len(data_objects) >= 1:
            data_obj = data_objects[0]
            if len(task_names) >= 1:
                task = task_names[0]
                actions = [f"{task}(start, {self.id_param})", f"{task}(end, {self.id_param})"]
                samples.append(ReasoningSample(
                    task_type=ReasoningTask.PROJECTION,
                    sample_id=5,
                    actions=actions,
                    fluent=f"{data_obj}({self.id_param})",
                    property_expr=None,
                    expected_result=ExpectedResult.TRUE,
                    description=f"Data object: '{data_obj}' exists after task execution"
                ))
        
        # Sample 6: Data object - FALSE (data object not created)
        if len(data_objects) >= 1:
            data_obj = data_objects[0]
            actions = []
            samples.append(ReasoningSample(
                task_type=ReasoningTask.PROJECTION,
                sample_id=6,
                actions=actions,
                fluent=f"{data_obj}({self.id_param})",
                property_expr=None,
                expected_result=ExpectedResult.FALSE,
                description=f"Data object: '{data_obj}' should not exist initially"
            ))
        
        return samples
    
    def generate_legality_samples(self) -> List[ReasoningSample]:
        """Generate 6 legality samples (mix of legal/illegal sequences).
        
        Returns:
            List of legality samples
        """
        samples = []
        task_names = self._get_sanitized_task_names()
        
        # Sample 1: LEGAL - Single task execution (start then end)
        if len(task_names) >= 1:
            task = task_names[0]
            actions = [f"{task}(start, {self.id_param})", f"{task}(end, {self.id_param})"]
            samples.append(ReasoningSample(
                task_type=ReasoningTask.LEGALITY,
                sample_id=1,
                actions=actions,
                fluent=None,
                property_expr=None,
                expected_result=ExpectedResult.SUCCESS,
                description=f"Legal: Complete execution of task '{task}'"
            ))
        
        # Sample 2: ILLEGAL - End task before start
        if len(task_names) >= 1:
            task = task_names[0]
            actions = [f"{task}(end, {self.id_param})"]
            samples.append(ReasoningSample(
                task_type=ReasoningTask.LEGALITY,
                sample_id=2,
                actions=actions,
                fluent=None,
                property_expr=None,
                expected_result=ExpectedResult.FAILURE,
                description=f"Illegal: End task '{task}' before starting"
            ))
        
        # Sample 3: LEGAL - Sequential execution of two tasks
        if len(task_names) >= 2:
            task1, task2 = task_names[0], task_names[1]
            actions = [
                f"{task1}(start, {self.id_param})", f"{task1}(end, {self.id_param})",
                f"{task2}(start, {self.id_param})", f"{task2}(end, {self.id_param})"
            ]
            samples.append(ReasoningSample(
                task_type=ReasoningTask.LEGALITY,
                sample_id=3,
                actions=actions,
                fluent=None,
                property_expr=None,
                expected_result=ExpectedResult.SUCCESS,
                description=f"Legal: Sequential execution of '{task1}' then '{task2}'"
            ))
        
        # Sample 4: ILLEGAL - Start task twice without ending
        if len(task_names) >= 1:
            task = task_names[0]
            actions = [f"{task}(start, {self.id_param})", f"{task}(start, {self.id_param})"]
            samples.append(ReasoningSample(
                task_type=ReasoningTask.LEGALITY,
                sample_id=4,
                actions=actions,
                fluent=None,
                property_expr=None,
                expected_result=ExpectedResult.FAILURE,
                description=f"Illegal: Start task '{task}' twice without ending"
            ))
        
        # Sample 5: LEGAL - Empty sequence
        samples.append(ReasoningSample(
            task_type=ReasoningTask.LEGALITY,
            sample_id=5,
            actions=[],
            fluent=None,
            property_expr=None,
            expected_result=ExpectedResult.SUCCESS,
            description="Legal: Empty action sequence (initial state)"
        ))
        
        # Sample 6: ILLEGAL - Execute middle task without prerequisites
        if len(task_names) >= 2:
            task = task_names[1]  # Typically requires previous task
            actions = [f"{task}(start, {self.id_param})"]
            samples.append(ReasoningSample(
                task_type=ReasoningTask.LEGALITY,
                sample_id=6,
                actions=actions,
                fluent=None,
                property_expr=None,
                expected_result=ExpectedResult.FAILURE,
                description=f"Illegal: Start '{task}' without prerequisites"
            ))
        
        return samples
    
    def generate_conformance_samples(self) -> List[ReasoningSample]:
        """Generate 6 conformance samples (mix of conforming/non-conforming).
        
        Returns:
            List of conformance samples
        """
        samples = []
        task_names = self._get_sanitized_task_names()
        start_events = self._get_sanitized_start_events()
        
        # Get the start event action (exogenous action that triggers process)
        start_event = start_events[0] if start_events else None
        
        # Use the first task in execution order if available, otherwise fall back to first in list
        first_task_raw = self.metrics.first_task_after_start
        first_task = self._sanitize_single_name(first_task_raw) if first_task_raw else (task_names[0] if task_names else None)
        
        # Sample 1: CONFORMS - Start event + first task in control flow
        if start_event and first_task:
            actions = [
                f"{start_event}({self.id_param})",
                f"{first_task}(start, {self.id_param})", 
                f"{first_task}(end, {self.id_param})"
            ]
            samples.append(ReasoningSample(
                task_type=ReasoningTask.CONFORMANCE,
                sample_id=1,
                actions=actions,
                fluent=None,
                property_expr=None,
                expected_result=ExpectedResult.CONFORMS,
                description=f"Conforms: Start event + valid execution of first task '{first_task}'"
            ))
        
        # Sample 2: NOT CONFORMS - Skip start event (use first task)
        if first_task:
            actions = [f"{first_task}(start, {self.id_param})", f"{first_task}(end, {self.id_param})"]
            samples.append(ReasoningSample(
                task_type=ReasoningTask.CONFORMANCE,
                sample_id=2,
                actions=actions,
                fluent=None,
                property_expr=None,
                expected_result=ExpectedResult.NOT_CONFORMS,
                description=f"Not conforms: Missing start event before first task '{first_task}'"
            ))
        
        # Sample 3: CONFORMS - Start event + sequential tasks
        if start_event and len(task_names) >= 2:
            task1, task2 = task_names[0], task_names[1]
            actions = [
                f"{start_event}({self.id_param})",
                f"{task1}(start, {self.id_param})", f"{task1}(end, {self.id_param})",
                f"{task2}(start, {self.id_param})", f"{task2}(end, {self.id_param})"
            ]
            samples.append(ReasoningSample(
                task_type=ReasoningTask.CONFORMANCE,
                sample_id=3,
                actions=actions,
                fluent=None,
                property_expr=None,
                expected_result=ExpectedResult.CONFORMS,
                description=f"Conforms: Start event + sequential execution '{task1}' → '{task2}'"
            ))
        
        # Sample 4: NOT CONFORMS - Wrong order (even with start event)
        if start_event and len(task_names) >= 2:
            task1, task2 = task_names[0], task_names[1]
            actions = [
                f"{start_event}({self.id_param})",
                f"{task2}(start, {self.id_param})", f"{task2}(end, {self.id_param})",
                f"{task1}(start, {self.id_param})", f"{task1}(end, {self.id_param})"
            ]
            samples.append(ReasoningSample(
                task_type=ReasoningTask.CONFORMANCE,
                sample_id=4,
                actions=actions,
                fluent=None,
                property_expr=None,
                expected_result=ExpectedResult.NOT_CONFORMS,
                description=f"Not conforms: Wrong order '{task2}' before '{task1}'"
            ))
        
        # Sample 5: CONFORMS - Start event + longer sequence
        if start_event and len(task_names) >= 3:
            task1, task2, task3 = task_names[0], task_names[1], task_names[2]
            actions = [
                f"{start_event}({self.id_param})",
                f"{task1}(start, {self.id_param})", f"{task1}(end, {self.id_param})",
                f"{task2}(start, {self.id_param})", f"{task2}(end, {self.id_param})",
                f"{task3}(start, {self.id_param})", f"{task3}(end, {self.id_param})"
            ]
            samples.append(ReasoningSample(
                task_type=ReasoningTask.CONFORMANCE,
                sample_id=5,
                actions=actions,
                fluent=None,
                property_expr=None,
                expected_result=ExpectedResult.CONFORMS,
                description=f"Conforms: Start event + longer sequence '{task1}' → '{task2}' → '{task3}'"
            ))
        
        # Sample 6: NOT CONFORMS - Incomplete execution (start without end)
        if start_event and len(task_names) >= 1:
            task = task_names[0]
            actions = [f"{start_event}({self.id_param})", f"{task}(start, {self.id_param})"]
            samples.append(ReasoningSample(
                task_type=ReasoningTask.CONFORMANCE,
                sample_id=6,
                actions=actions,
                fluent=None,
                property_expr=None,
                expected_result=ExpectedResult.NOT_CONFORMS,
                description=f"Not conforms: Incomplete execution of '{task}' (no end action)"
            ))
        
        return samples
    
    def generate_property_verification_samples(self) -> List[ReasoningSample]:
        """Generate 6 property verification samples.
        
        Returns:
            List of property verification samples
        """
        samples = []
        task_names = self._get_sanitized_task_names()
        data_objects = self._get_sanitized_data_objects()
        
        # Sample 1: Property - "Eventually all tasks complete"
        if len(task_names) >= 1:
            task = task_names[0]
            property_expr = f"done({task}(end, id))"
            samples.append(ReasoningSample(
                task_type=ReasoningTask.PROPERTY_VERIFICATION,
                sample_id=1,
                actions=[],
                fluent=None,
                property_expr=property_expr,
                expected_result=ExpectedResult.SUCCESS,
                description=f"Property: Eventually task '{task}' completes"
            ))
        
        # Sample 2: Property - "Data object is created"
        if len(data_objects) >= 1 and len(task_names) >= 1:
            data_obj = data_objects[0]
            property_expr = f"{data_obj}(id)"
            samples.append(ReasoningSample(
                task_type=ReasoningTask.PROPERTY_VERIFICATION,
                sample_id=2,
                actions=[],
                fluent=None,
                property_expr=property_expr,
                expected_result=ExpectedResult.SUCCESS,
                description=f"Property: Data object '{data_obj}' is created"
            ))
        
        # Sample 3: Property - "Task completed before another"
        if len(task_names) >= 2:
            task1, task2 = task_names[0], task_names[1]
            property_expr = f"and(done({task1}(end, id)), done({task2}(end, id)))"
            samples.append(ReasoningSample(
                task_type=ReasoningTask.PROPERTY_VERIFICATION,
                sample_id=3,
                actions=[],
                fluent=None,
                property_expr=property_expr,
                expected_result=ExpectedResult.SUCCESS,
                description=f"Property: Both '{task1}' and '{task2}' complete"
            ))
        
        # Sample 4: Property - "Mutual exclusion" (should fail in many processes)
        if len(task_names) >= 2:
            task1, task2 = task_names[0], task_names[1]
            property_expr = f"and(done({task1}(end, id)), neg(done({task2}(end, id))))"
            samples.append(ReasoningSample(
                task_type=ReasoningTask.PROPERTY_VERIFICATION,
                sample_id=4,
                actions=[],
                fluent=None,
                property_expr=property_expr,
                expected_result=ExpectedResult.SUCCESS,
                description=f"Property: '{task1}' completes but not '{task2}'"
            ))
        
        # Sample 5: Property - "Process can complete"
        if len(task_names) >= 1:
            last_task = task_names[-1]
            property_expr = f"done({last_task}(end, id))"
            samples.append(ReasoningSample(
                task_type=ReasoningTask.PROPERTY_VERIFICATION,
                sample_id=5,
                actions=[],
                fluent=None,
                property_expr=property_expr,
                expected_result=ExpectedResult.SUCCESS,
                description=f"Property: Process completes (last task '{last_task}')"
            ))
        
        # Sample 6: Property - "Impossible state" (should fail)
        if len(task_names) >= 1:
            task = task_names[0]
            property_expr = f"and(done({task}(end, id)), neg(done({task}(start, id))))"
            samples.append(ReasoningSample(
                task_type=ReasoningTask.PROPERTY_VERIFICATION,
                sample_id=6,
                actions=[],
                fluent=None,
                property_expr=property_expr,
                expected_result=ExpectedResult.FAILURE,
                description=f"Property: Impossible - '{task}' ends without starting"
            ))
        
        return samples
    
    def _sanitize_single_name(self, name: str) -> str:
        """Sanitize a single name to be Prolog-compatible.
        
        Args:
            name: Name to sanitize
            
        Returns:
            Sanitized name
        """
        # Convert to snake_case and remove special characters
        clean = re.sub(r'[^\w\s]', '', name.lower())
        clean = re.sub(r'\s+', '_', clean.strip())
        return clean
    
    def _get_sanitized_task_names(self) -> List[str]:
        """Get sanitized task names suitable for Prolog.
        
        Returns:
            List of sanitized task names
        """
        sanitized = []
        for name in self.metrics.task_names:
            clean = self._sanitize_single_name(name)
            if clean and clean not in sanitized:
                sanitized.append(clean)
        return sanitized
    
    def _get_sanitized_gateway_conditions(self) -> List[str]:
        """Get sanitized gateway condition names.
        
        Returns:
            List of sanitized condition names
        """
        sanitized = []
        for cond in self.metrics.gateway_conditions:
            # Extract key terms and convert to snake_case
            clean = re.sub(r'[^\w\s]', '', cond.lower())
            clean = re.sub(r'\s+', '_', clean.strip())
            # Often gateway conditions are questions, extract key term
            words = clean.split('_')
            if len(words) > 0:
                # Use first meaningful word
                key_word = next((w for w in words if len(w) > 2), words[0])
                if key_word and key_word not in sanitized:
                    sanitized.append(key_word)
        return sanitized
    
    def _get_sanitized_start_events(self) -> List[str]:
        """Get sanitized start event names.
        
        Returns:
            List of sanitized start event names
        """
        sanitized = []
        for name in self.metrics.start_event_names:
            # Convert to snake_case and remove special characters
            clean = re.sub(r'[^\w\s]', '', name.lower())
            clean = re.sub(r'\s+', '_', clean.strip())
            if clean and clean not in sanitized:
                sanitized.append(clean)
        return sanitized
    
    def _get_sanitized_data_objects(self) -> List[str]:
        """Get sanitized data object names.
        
        Returns:
            List of sanitized data object names
        """
        sanitized = []
        for obj in self.metrics.data_objects:
            clean = re.sub(r'[^\w\s]', '', obj.lower())
            clean = re.sub(r'\s+', '_', clean.strip())
            if clean and clean not in sanitized:
                sanitized.append(clean)
        return sanitized


if __name__ == '__main__':
    import sys
    from bpmn_metrics import extract_bpmn_metrics
    
    if len(sys.argv) < 2:
        print("Usage: python sample_generator.py <bpmn_file_path>")
        sys.exit(1)
    
    bpmn_file = sys.argv[1]
    model_name = bpmn_file.split('/')[-1].replace('.bpmn', '').replace('.xml', '')
    
    print(f"\nGenerating samples for: {model_name}")
    print("=" * 70)
    
    metrics = extract_bpmn_metrics(bpmn_file)
    generator = SampleGenerator(model_name, metrics)
    samples = generator.generate_all_samples()
    
    print(f"\nGenerated {len(samples)} samples:")
    for task_type in ReasoningTask:
        task_samples = [s for s in samples if s.task_type == task_type]
        print(f"\n{task_type.value}: {len(task_samples)} samples")
        for sample in task_samples[:2]:  # Show first 2 of each type
            print(f"  - Sample {sample.sample_id}: {sample.description}")
