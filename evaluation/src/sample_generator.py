"""
Sample Generator for BPMN Evaluation

This module generates test samples for different reasoning tasks:
- Projection: 4 samples (2 true, 2 false) testing control flow fluents
- Legality: 4 samples (2 legal, 2 illegal) testing action sequences
- Conformance: 4 samples (2 conforming, 2 non-conforming) testing trace compliance
- Property Verification: 4 samples (2 success, 2 failure) testing temporal properties
"""

import re
import random
import itertools
import os
from dataclasses import dataclass
from typing import List, Tuple, Optional
from enum import Enum
from bpmn_metrics import BPMNMetrics

RANDOM_SEED = 42
random.seed(RANDOM_SEED)


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
            'actions': ';'.join(self.actions) if self.actions else '',  # Use semicolon to avoid CSV parsing issues
            'fluent': self.fluent or '',
            'property_expr': self.property_expr or '',
            'expected_result': self.expected_result.value,
            'description': self.description
        }


class SampleGenerator:
    """Generate test samples for BPMN reasoning evaluation."""
    
    def __init__(self, model_name: str, bpmn_metrics: BPMNMetrics, prolog_model_path: str = None):
        """Initialize the sample generator.
        
        Args:
            model_name: Name of the BPMN model
            bpmn_metrics: Extracted BPMN metrics
            prolog_model_path: Optional path to the Prolog model file to extract gateway fluents
        """
        self.model_name = model_name
        self.metrics = bpmn_metrics
        self.id_param = "1"  # Default ID parameter for actions
        self.prolog_model_path = prolog_model_path
        self._gateway_fluents_from_prolog = None
    
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
        """Generate 4 projection samples (2 true, 2 false).
        
        Returns:
            List of projection samples
        """
        samples = []
        
        # Get task names for control flow fluents
        task_names = self._get_sanitized_task_names()
        
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
        if len(task_names) >= 1:
            task = task_names[0]
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
        
        # Sample 3: Control flow - TRUE (different task is done)
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
        
        # Sample 4: Control flow - FALSE (different task not done initially)
        if len(task_names) >= 2:
            task = task_names[1]
            actions = []
            samples.append(ReasoningSample(
                task_type=ReasoningTask.PROJECTION,
                sample_id=4,
                actions=actions,
                fluent=f"done({task}(end, {self.id_param}))",
                property_expr=None,
                expected_result=ExpectedResult.FALSE,
                description=f"Control flow: Task '{task}' should not be done initially"
            ))
        
        return samples
    
    def generate_legality_samples(self) -> List[ReasoningSample]:
        """Generate 4 legality samples (2 legal, 2 illegal sequences).
        
        Returns:
            List of legality samples
        """
        samples = []
        task_names = self._get_sanitized_task_names()
        start_events = self._get_sanitized_start_events()
        
        # Get the start event action (exogenous action that triggers process)
        start_event = start_events[0] if start_events else None
        
        # Get the first task in true execution order (after start event)
        first_task_raw = self.metrics.first_task_after_start
        first_task = self._sanitize_single_name(first_task_raw) if first_task_raw else (task_names[0] if task_names else None)
        
        # Sample 1: LEGAL - Start event + single task execution (start then end)
        if start_event and first_task:
            actions = [f"{start_event}({self.id_param})", f"{first_task}(start, {self.id_param})", f"{first_task}(end, {self.id_param})"]
            samples.append(ReasoningSample(
                task_type=ReasoningTask.LEGALITY,
                sample_id=1,
                actions=actions,
                fluent=None,
                property_expr=None,
                expected_result=ExpectedResult.SUCCESS,
                description=f"Legal: Start event + complete execution of task '{first_task}'"
            ))
        
        # Sample 2: ILLEGAL - End task before start (even with start event)
        if start_event and first_task:
            actions = [f"{start_event}({self.id_param})", f"{first_task}(end, {self.id_param})"]
            samples.append(ReasoningSample(
                task_type=ReasoningTask.LEGALITY,
                sample_id=2,
                actions=actions,
                fluent=None,
                property_expr=None,
                expected_result=ExpectedResult.FAILURE,
                description=f"Illegal: End task '{first_task}' before starting it"
            ))
        
        # Sample 3: LEGAL - Start event + sequential execution of two tasks
        if start_event and len(task_names) >= 2:
            task1 = first_task if first_task else task_names[0]
            task2 = task_names[1] if task_names[1] != task1 else (task_names[2] if len(task_names) > 2 else task_names[0])
            actions = [
                f"{start_event}({self.id_param})",
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
                description=f"Legal: Start event + sequential execution of '{task1}' then '{task2}'"
            ))
        
        # Sample 4: ILLEGAL - Start task twice without ending (with start event)
        if start_event and first_task:
            actions = [f"{start_event}({self.id_param})", f"{first_task}(start, {self.id_param})", f"{first_task}(start, {self.id_param})"]
            samples.append(ReasoningSample(
                task_type=ReasoningTask.LEGALITY,
                sample_id=4,
                actions=actions,
                fluent=None,
                property_expr=None,
                expected_result=ExpectedResult.FAILURE,
                description=f"Illegal: Start task '{first_task}' twice without ending"
            ))
        
        return samples
    
    def generate_conformance_samples(self) -> List[ReasoningSample]:
        """Generate 4 conformance samples (2 conforming, 2 non-conforming).
        
        NOTE: Conformance histories must be in REVERSE order and include acquire(ID, POOL) actions.
        
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
        
        # Default pool name (matches translator output)
        pool_name = "main_process"
        
        # Get sanitized parallel branches
        parallel_branches = []
        if self.metrics.parallel_branches:
            for branch in self.metrics.parallel_branches:
                sanitized_branch = [self._sanitize_single_name(t) for t in branch if self._sanitize_single_name(t)]
                if len(sanitized_branch) >= 2:
                    parallel_branches.append(sanitized_branch)
        
        # Sample 1: CONFORMS - Start event + complete execution of first 3 tasks
        # Use original order (permutation 0)
        if start_event and len(task_names) >= 1:
            # Get task sequence with default permutation
            ordered_tasks = self._get_task_sequence_with_parallel_permutation(task_names, parallel_branches, 0)
            num_tasks = min(3, len(ordered_tasks))
            
            # Build actions in FORWARD execution order first
            actions = [f"{start_event}({self.id_param})"]
            actions.append(f"acquire({self.id_param}, {pool_name})")
            # Include first 3 tasks in sequence for a conformant trace
            for task in ordered_tasks[:num_tasks]:
                actions.append(f"{task}(start, {self.id_param})")
                actions.append(f"{task}(end, {self.id_param})")
            # REVERSE the entire list for conformance checking (history is in reverse)
            actions.reverse()
            task_list = ' → '.join(ordered_tasks[:num_tasks])
            samples.append(ReasoningSample(
                task_type=ReasoningTask.CONFORMANCE,
                sample_id=1,
                actions=actions,
                fluent=None,
                property_expr=None,
                expected_result=ExpectedResult.CONFORMS,
                description=f"Conforms: Start event + first {num_tasks} task(s): {task_list}"
            ))
        
        # Sample 2: NOT CONFORMS - Skip start event (use first task) - NO ACQUIRE
        if first_task:
            # In reverse order: end, then start
            actions = [f"{first_task}(end, {self.id_param})", f"{first_task}(start, {self.id_param})"]
            samples.append(ReasoningSample(
                task_type=ReasoningTask.CONFORMANCE,
                sample_id=2,
                actions=actions,
                fluent=None,
                property_expr=None,
                expected_result=ExpectedResult.NOT_CONFORMS,
                description=f"Not conforms: Missing start event before first task '{first_task}'"
            ))
        
        # Sample 3: CONFORMS - Start event + first 3 tasks with alternate permutation for parallel tasks
        # Use permutation 1 (alternate order for parallel tasks, if any)
        if start_event and len(task_names) >= 1:
            # Get task sequence with alternate permutation if parallel branches exist
            ordered_tasks = self._get_task_sequence_with_parallel_permutation(task_names, parallel_branches, 1)
            num_tasks = min(3, len(ordered_tasks))
            
            # Build in forward order
            actions = [f"{start_event}({self.id_param})"]
            actions.append(f"acquire({self.id_param}, {pool_name})")
            for task in ordered_tasks[:num_tasks]:
                actions.append(f"{task}(start, {self.id_param})")
                actions.append(f"{task}(end, {self.id_param})")
            # Reverse for conformance
            actions.reverse()
            task_list = ' → '.join(ordered_tasks[:num_tasks])
            samples.append(ReasoningSample(
                task_type=ReasoningTask.CONFORMANCE,
                sample_id=3,
                actions=actions,
                fluent=None,
                property_expr=None,
                expected_result=ExpectedResult.CONFORMS,
                description=f"Conforms: Start event + first {num_tasks} task(s): {task_list}"
            ))
        
        # Sample 4: NOT CONFORMS - Wrong order (even with start event)
        if start_event and len(task_names) >= 2:
            task1 = first_task if first_task else task_names[0]
            task2 = task_names[1] if task_names[1] != task1 else (task_names[2] if len(task_names) > 2 else task_names[0])
            # Build in WRONG forward order: task2 before task1
            actions = [
                f"{start_event}({self.id_param})",
                f"acquire({self.id_param}, {pool_name})",
                f"{task2}(start, {self.id_param})", f"{task2}(end, {self.id_param})",
                f"{task1}(start, {self.id_param})", f"{task1}(end, {self.id_param})"
            ]
            # Reverse for conformance
            actions.reverse()
            samples.append(ReasoningSample(
                task_type=ReasoningTask.CONFORMANCE,
                sample_id=4,
                actions=actions,
                fluent=None,
                property_expr=None,
                expected_result=ExpectedResult.NOT_CONFORMS,
                description=f"Not conforms: Wrong order '{task2}' before '{task1}'"
            ))
        
        return samples
    
    def generate_property_verification_samples(self) -> List[ReasoningSample]:
        """Generate 4 property verification samples:
        - 2 samples testing control flow (done conditions)
        - 2 samples testing gateway values (functional or relational/boolean)
        
        Returns:
            List of property verification samples
        """
        samples = []
        task_names = self._get_sanitized_task_names()
        
        # Get functional and relational gateway fluents separately
        fun_fluents = self._get_functional_gateway_fluents()
        rel_fluents = self._get_relational_gateway_fluents()
        
        # Sample 1: Control Flow - "Eventually task completes" (SUCCESS)
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
                description=f"Control flow property: Eventually task '{task}' completes"
            ))
        
        # Sample 2: Control Flow - "Two tasks complete" (SUCCESS)
        if len(task_names) >= 2:
            task1, task2 = task_names[0], task_names[1]
            property_expr = f"and(done({task1}(end, id)), done({task2}(end, id)))"
            samples.append(ReasoningSample(
                task_type=ReasoningTask.PROPERTY_VERIFICATION,
                sample_id=2,
                actions=[],
                fluent=None,
                property_expr=property_expr,
                expected_result=ExpectedResult.SUCCESS,
                description=f"Control flow property: Both '{task1}' and '{task2}' complete"
            ))
        
        # Sample 3: Gateway - Functional fluent can have a specific value (SUCCESS)
        if len(fun_fluents) >= 1:
            gateway = fun_fluents[0]
            # For functional fluents, we use: some(val, fluent(id) = val)
            property_expr = f"some(val, {gateway}(id) = val)"
            samples.append(ReasoningSample(
                task_type=ReasoningTask.PROPERTY_VERIFICATION,
                sample_id=3,
                actions=[],
                fluent=None,
                property_expr=property_expr,
                expected_result=ExpectedResult.SUCCESS,
                description=f"Gateway property: Functional fluent '{gateway}' can have a value"
            ))
        elif len(rel_fluents) >= 1:
            # Fallback to relational fluent if no functional fluents
            gateway = rel_fluents[0]
            property_expr = f"{gateway}(id)"
            samples.append(ReasoningSample(
                task_type=ReasoningTask.PROPERTY_VERIFICATION,
                sample_id=3,
                actions=[],
                fluent=None,
                property_expr=property_expr,
                expected_result=ExpectedResult.SUCCESS,
                description=f"Gateway property: Relational fluent '{gateway}' can be true"
            ))
        
        # Sample 4: Gateway - Relational fluent or negation (SUCCESS)
        if len(rel_fluents) >= 1:
            gateway = rel_fluents[0]
            property_expr = f"{gateway}(id)"
            samples.append(ReasoningSample(
                task_type=ReasoningTask.PROPERTY_VERIFICATION,
                sample_id=4,
                actions=[],
                fluent=None,
                property_expr=property_expr,
                expected_result=ExpectedResult.SUCCESS,
                description=f"Gateway property: Relational fluent '{gateway}' can be true"
            ))
        elif len(fun_fluents) >= 2:
            # Use a second functional fluent if available
            gateway = fun_fluents[1]
            property_expr = f"some(val, {gateway}(id) = val)"
            samples.append(ReasoningSample(
                task_type=ReasoningTask.PROPERTY_VERIFICATION,
                sample_id=4,
                actions=[],
                fluent=None,
                property_expr=property_expr,
                expected_result=ExpectedResult.SUCCESS,
                description=f"Gateway property: Functional fluent '{gateway}' can have a value"
            ))
        
        return samples
    
    def _sanitize_single_name(self, name: str) -> str:
        """Sanitize a single name to be Prolog-compatible.
        
        This method replicates the _prologify() logic from prolog_translator.py
        to ensure names in samples match those in generated Prolog code.
        
        Args:
            name: Name to sanitize
            
        Returns:
            Sanitized name
        """
        if not name:
            return ""
        
        s = name.lower()
        # Character replacements (matching _prologify)
        s = s.replace('>', 'larger')
        s = s.replace('<', 'smaller')
        s = s.replace('=', 'equal')
        s = s.replace('+', 'and')
        s = s.replace('ß', 'ss')
        s = s.replace('ä', 'a')
        s = s.replace('ö', 'o')
        s = s.replace('ü', 'u')
        s = s.strip(' ')
        # Replace whitespace and hyphens with underscores
        s = re.sub(r'[\s-]+', '_', s)
        # Remove special characters
        s = re.sub(r'[?\'\"!@#$%^*+~`|\\/:;&\(\)\[\]{},.]', '', s)
        
        # Handle send_ prefix (convert to _sent suffix)
        if s.startswith('send_'):
            s = s.replace('send_', '', 1) + '_sent'
        
        # Ensure first character is lowercase or add prefix
        if s and not s[0].islower():
            if s[0].isdigit():
                s = 'n' + s  # 'n' for numeric
            elif s[0] == '_':
                s = 'v' + s  # 'v' for value
            else:
                s = 'x' + s  # 'x' for other
        
        return s
    
    def _get_task_sequence_with_parallel_permutation(self, task_names: List[str], parallel_branches: List[List[str]], permutation_index: int = 0) -> List[str]:
        """Get a task sequence that respects parallel branch permutations.
        
        Args:
            task_names: Original task sequence
            parallel_branches: List of parallel task groups
            permutation_index: Which permutation to use (0 for original order)
            
        Returns:
            Task sequence with parallel branches permuted
        """
        if not parallel_branches:
            return task_names.copy()
        
        # Build a set of all parallel tasks
        parallel_tasks = set()
        for branch in parallel_branches:
            parallel_tasks.update(branch)
        
        # Split tasks into sequential and parallel groups
        result = []
        i = 0
        while i < len(task_names):
            task = task_names[i]
            
            # Check if this task is part of a parallel group
            found_parallel_group = None
            for branch in parallel_branches:
                if task in branch:
                    found_parallel_group = branch
                    break
            
            if found_parallel_group:
                # Collect all tasks from this parallel group that appear in task_names
                parallel_group_tasks = []
                for t in task_names[i:]:
                    if t in found_parallel_group:
                        parallel_group_tasks.append(t)
                        if len(parallel_group_tasks) == len(found_parallel_group):
                            break
                
                # Generate permutations and pick the one at permutation_index
                permutations = list(itertools.permutations(parallel_group_tasks))
                perm_idx = permutation_index % len(permutations) if permutations else 0
                result.extend(permutations[perm_idx])
                
                # Skip past all tasks in this parallel group
                skip_count = len(parallel_group_tasks)
                i += skip_count
            else:
                # Sequential task
                result.append(task)
                i += 1
        
        return result
    
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
    
    def _extract_gateway_fluents_from_prolog(self) -> Tuple[List[str], List[str]]:
        """Extract gateway fluents from the Prolog model file.
        
        Returns:
            Tuple of (functional_fluents, relational_fluents) - gateway fluent names without (ID)
        """
        if not self.prolog_model_path or not os.path.exists(self.prolog_model_path):
            return [], []
        
        functional_fluents = []
        relational_fluents = []
        try:
            with open(self.prolog_model_path, 'r') as f:
                content = f.read()
                # Look for patterns like: fun_fluent(choice_xxx_or_yyy(_ID))
                fun_pattern = r'fun_fluent\((choice_[a-z_]+)\(_ID\)\)'
                functional_fluents = re.findall(fun_pattern, content)
                
                # Look for patterns like: rel_fluent(choice_xxx(_ID))
                rel_pattern = r'rel_fluent\((choice_[a-z_]+)\(_ID\)\)'
                relational_fluents = re.findall(rel_pattern, content)
        except Exception as e:
            print(f"Warning: Could not extract gateway fluents from Prolog file: {e}")
        
        return list(set(functional_fluents)), list(set(relational_fluents))
    
    def _get_sanitized_gateway_conditions(self) -> List[str]:
        """Get sanitized gateway condition names.
        
        First tries to get them from BPMN metrics, then falls back to extracting from Prolog model.
        
        Returns:
            List of sanitized condition names
        """
        # First try from BPMN metrics
        sanitized = []
        for cond in self.metrics.gateway_conditions:
            # Use the same sanitization as for task names
            clean = self._sanitize_single_name(cond)
            if clean and clean not in sanitized:
                sanitized.append(clean)
        
        # If no conditions found and we have a Prolog model path, extract from there
        if not sanitized and self.prolog_model_path:
            if self._gateway_fluents_from_prolog is None:
                fun_fluents, rel_fluents = self._extract_gateway_fluents_from_prolog()
                self._gateway_fluents_from_prolog = (fun_fluents, rel_fluents)
            fun_fluents, rel_fluents = self._gateway_fluents_from_prolog
            sanitized = fun_fluents + rel_fluents
        
        return sanitized
    
    def _get_functional_gateway_fluents(self) -> List[str]:
        """Get functional gateway fluents from Prolog model.
        
        Returns:
            List of functional gateway fluent names
        """
        if self._gateway_fluents_from_prolog is None:
            fun_fluents, rel_fluents = self._extract_gateway_fluents_from_prolog()
            self._gateway_fluents_from_prolog = (fun_fluents, rel_fluents)
        return self._gateway_fluents_from_prolog[0]
    
    def _get_relational_gateway_fluents(self) -> List[str]:
        """Get relational gateway fluents from Prolog model.
        
        Returns:
            List of relational gateway fluent names
        """
        if self._gateway_fluents_from_prolog is None:
            fun_fluents, rel_fluents = self._extract_gateway_fluents_from_prolog()
            self._gateway_fluents_from_prolog = (fun_fluents, rel_fluents)
        return self._gateway_fluents_from_prolog[1]
    
    def _get_sanitized_start_events(self) -> List[str]:
        """Get sanitized start event names.
        
        Returns:
            List of sanitized start event names
        """
        sanitized = []
        for name in self.metrics.start_event_names:
            clean = self._sanitize_single_name(name)
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
            clean = self._sanitize_single_name(obj)
            if clean and clean not in sanitized:
                sanitized.append(clean)
        return sanitized


if __name__ == '__main__':
    import sys
    import csv
    from pathlib import Path
    from bpmn_metrics import extract_bpmn_metrics
    
    if len(sys.argv) < 3:
        print("Usage: python sample_generator.py <bpmn_file_path> <model_name>")
        sys.exit(1)
    
    bpmn_file = sys.argv[1]
    model_name = sys.argv[2]
    
    print(f"\nGenerating samples for: {model_name}")
    print("=" * 70)
    
    metrics = extract_bpmn_metrics(bpmn_file)
    
    # Try to find the Prolog model file
    prolog_model_path = None
    possible_paths = [
        Path(__file__).parent.parent.parent / 'pl_models' / model_name / f'{model_name}.pl',
        Path(__file__).parent.parent.parent / 'pl_models' / 'case_study' / f'{model_name}.pl'
    ]
    for path in possible_paths:
        if path.exists():
            prolog_model_path = str(path)
            print(f"Found Prolog model: {prolog_model_path}")
            break
    
    generator = SampleGenerator(model_name, metrics, prolog_model_path)
    samples = generator.generate_all_samples()
    
    print(f"\nGenerated {len(samples)} samples:")
    for task_type in ReasoningTask:
        task_samples = [s for s in samples if s.task_type == task_type]
        print(f"\n{task_type.value}: {len(task_samples)} samples")
        for sample in task_samples[:2]:  # Show first 2 of each type
            print(f"  - Sample {sample.sample_id}: {sample.description}")
    
    # Write to CSV
    output_path = Path(__file__).parent.parent / 'datasets' / f'{model_name}_samples.csv'
    output_path.parent.mkdir(parents=True, exist_ok=True)
    
    with open(output_path, 'w', newline='') as f:
        writer = csv.DictWriter(f, fieldnames=samples[0].to_dict().keys())
        writer.writeheader()
        for sample in samples:
            writer.writerow(sample.to_dict())
    
    print(f"\n✓ Samples written to: {output_path}")
