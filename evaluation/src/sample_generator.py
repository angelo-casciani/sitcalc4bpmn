"""
Sample Generator for BPMN Evaluation

This module generates test samples for different reasoning tasks:
- Legality: samples testing if action sequences are legal (true) or illegal (false)
- Conformance: samples testing if traces are conformant (true) or not conformant (false)
"""
import csv
from bpmn_metrics import extract_bpmn_metrics, BPMNMetrics
import re
import random
import os
from typing import List, Tuple, Dict
from trace_generator import SimpleBPMNSimulator

RANDOM_SEED = 42
random.seed(RANDOM_SEED)


class SampleGenerator:    
    def __init__(self, model_name: str, bpmn_metrics: BPMNMetrics, prolog_model_path: str = None):
        self.model_name = model_name
        self.metrics = bpmn_metrics
        self.prolog_model_path = prolog_model_path
    
    def _convert_to_indigolog_actions(self, trace_with_types, trace_percentage=1.0, include_acquire=False):
        """Convert trace names to IndiGolog format with start/end pairs.
        
        Args:
            trace_with_types: List of (activity_name, element_type, xor_value) tuples from trace
            trace_percentage: Percentage of trace to use (0.0 to 1.0), e.g., 0.25 for 25%, 0.75 for 75%
            include_acquire: If True, include acquire(1, main_process) action after start event
        
        Returns:
            List of IndiGolog-formatted action strings
        """
        import math
        
        if not trace_with_types:
            return ['start_event(1)']
        
        # Calculate trace length based on percentage
        trace_len = max(1, int(len(trace_with_types) * trace_percentage))
        selected_trace = trace_with_types[:trace_len]
        
        indigolog_actions = []
        
        # The first element should be the start event (exogenous action without start/end)
        # Format: event_name(1)
        # For conformance checking, we also need to add the acquire(1, pool_name) action
        # that the Prolog interpreter automatically adds after the start event
        if selected_trace:
            first_name, first_type, first_xor = selected_trace[0]
            indigolog_actions.append(f'{first_name}(1)')
            
            # Add acquire action if requested (for conformance checking)
            if include_acquire:
                indigolog_actions.append('acquire(1, main_process)')
            
            remaining = selected_trace[1:]
        else:
            remaining = []
        
        # Convert each activity to start/end pair based on BPMN element type
        for activity_name, element_type, xor_value in remaining:
            # Check if this is an event based on BPMN element type
            # Events (start, end, intermediate, boundary) don't have start/end
            # Tasks (task, userTask, serviceTask, manualTask, etc.) have start/end
            is_event = 'Event' in element_type
            
            if is_event:
                # Events are just: event_name(1)
                indigolog_actions.append(f'{activity_name}(1)')
            else:
                # Regular activities (tasks): activity(start,1) then activity(end,1)
                indigolog_actions.append(f'{activity_name}(start,1)')
                
                # If this activity precedes an XOR gateway, the end action gets a third argument
                if xor_value:
                    indigolog_actions.append(f'{activity_name}(end,1,{xor_value})')
                else:
                    indigolog_actions.append(f'{activity_name}(end,1)')
        
        return indigolog_actions
    
    def generate_samples_from_traces(self, traces_with_types: List[List[Tuple]], model_name: str) -> List[Dict]:
        """Generate exactly 8 samples (4 legality + 4 conformance) from traces.
        
        Generates:
        - Legality: 2 small (25%), 2 large (50%) - 1 TRUE + 1 FALSE each
        - Conformance: 2 small (25%), 2 large (50%) - 1 TRUE + 1 FALSE each
        
        Distributes samples across different traces when available.
        
        Args:
            traces_with_types: List of traces, each trace is a list of (activity_name, element_type, xor_value) tuples
            model_name: Name of the model
            
        Returns:
            List of exactly 8 samples with IndiGolog-formatted actions
        """
        samples = []
        
        if not traces_with_types:
            return samples
        
        # We need 4 traces ideally (one for each pair), but can work with fewer
        # Distribute: trace 0 -> small samples, trace 1 -> large samples, etc.
        num_traces = len(traces_with_types)
        
        # Helper function to get trace by index (wrap around if needed)
        def get_trace(idx):
            return traces_with_types[idx % num_traces]
        
        # Helper function to create swapped trace (swap activities at positions 1 and 2)
        def create_swapped_trace(trace):
            if len(trace) < 3:
                return trace  # Can't swap if too short
            swapped = list(trace)
            swapped[1], swapped[2] = swapped[2], swapped[1]
            return swapped
        
        # Sanitize traces
        sanitized_traces = [
            [(self._sanitize_single_name(name), elem_type, xor_val) 
             for name, elem_type, xor_val in trace]
            for trace in traces_with_types
        ]
        
        # Sample 1: Legality TRUE - Small (25%)
        trace_0 = get_trace(0)
        sanitized_0 = sanitized_traces[0 % num_traces]
        actions = self._convert_to_indigolog_actions(sanitized_0, trace_percentage=0.25, include_acquire=False)
        samples.append({
            'task_type': 'legality',
            'sample_id': 1,
            'actions': ';'.join(actions) if actions else '',
            'expected_result': True,
            'model_name': model_name,
            'description': 'legality_small_true'
        })
        
        # Sample 2: Legality FALSE - Small (25%, swapped)
        trace_1 = get_trace(1)
        sanitized_1 = sanitized_traces[1 % num_traces]
        swapped_1 = create_swapped_trace(sanitized_1)
        actions = self._convert_to_indigolog_actions(swapped_1, trace_percentage=0.25, include_acquire=False)
        samples.append({
            'task_type': 'legality',
            'sample_id': 2,
            'actions': ';'.join(actions) if actions else '',
            'expected_result': False,
            'model_name': model_name,
            'description': 'legality_small_false'
        })
        
        # Sample 3: Legality TRUE - Large (50%)
        trace_2 = get_trace(2)
        sanitized_2 = sanitized_traces[2 % num_traces]
        actions = self._convert_to_indigolog_actions(sanitized_2, trace_percentage=0.50, include_acquire=False)
        samples.append({
            'task_type': 'legality',
            'sample_id': 3,
            'actions': ';'.join(actions) if actions else '',
            'expected_result': True,
            'model_name': model_name,
            'description': 'legality_large_true'
        })
        
        # Sample 4: Legality FALSE - Large (50%, swapped)
        trace_3 = get_trace(3)
        sanitized_3 = sanitized_traces[3 % num_traces]
        swapped_3 = create_swapped_trace(sanitized_3)
        actions = self._convert_to_indigolog_actions(swapped_3, trace_percentage=0.50, include_acquire=False)
        samples.append({
            'task_type': 'legality',
            'sample_id': 4,
            'actions': ';'.join(actions) if actions else '',
            'expected_result': False,
            'model_name': model_name,
            'description': 'legality_large_false'
        })
        
        # Sample 5: Conformance TRUE - Small (25%, with acquire)
        trace_4 = get_trace(4)
        sanitized_4 = sanitized_traces[4 % num_traces]
        actions = self._convert_to_indigolog_actions(sanitized_4, trace_percentage=0.25, include_acquire=True)
        samples.append({
            'task_type': 'conformance',
            'sample_id': 5,
            'actions': ';'.join(actions) if actions else '',
            'expected_result': True,
            'model_name': model_name,
            'description': 'conformance_small_true'
        })
        
        # Sample 6: Conformance FALSE - Small (25%, swapped, with acquire)
        trace_5 = get_trace(5)
        sanitized_5 = sanitized_traces[5 % num_traces]
        swapped_5 = create_swapped_trace(sanitized_5)
        actions = self._convert_to_indigolog_actions(swapped_5, trace_percentage=0.25, include_acquire=True)
        samples.append({
            'task_type': 'conformance',
            'sample_id': 6,
            'actions': ';'.join(actions) if actions else '',
            'expected_result': False,
            'model_name': model_name,
            'description': 'conformance_small_false'
        })
        
        # Sample 7: Conformance TRUE - Large (50%, with acquire)
        trace_6 = get_trace(6)
        sanitized_6 = sanitized_traces[6 % num_traces]
        actions = self._convert_to_indigolog_actions(sanitized_6, trace_percentage=0.50, include_acquire=True)
        samples.append({
            'task_type': 'conformance',
            'sample_id': 7,
            'actions': ';'.join(actions) if actions else '',
            'expected_result': True,
            'model_name': model_name,
            'description': 'conformance_large_true'
        })
        
        # Sample 8: Conformance FALSE - Large (50%, swapped, with acquire)
        trace_7 = get_trace(7)
        sanitized_7 = sanitized_traces[7 % num_traces]
        swapped_7 = create_swapped_trace(sanitized_7)
        actions = self._convert_to_indigolog_actions(swapped_7, trace_percentage=0.50, include_acquire=True)
        samples.append({
            'task_type': 'conformance',
            'sample_id': 8,
            'actions': ';'.join(actions) if actions else '',
            'expected_result': False,
            'model_name': model_name,
            'description': 'conformance_large_false'
        })
        
        return samples
    
    
    def _sanitize_single_name(self, name: str) -> str:
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
        # Ensure first character is lowercase or add prefix
        if s and not s[0].islower():
            if s[0].isdigit():
                s = 'n' + s  # 'n' for numeric
            elif s[0] == '_':
                s = 'v' + s  # 'v' for value
            else:
                s = 'x' + s  # 'x' for other
        
        return s
    
    def _get_sanitized_start_events(self) -> List[str]:
        sanitized = []
        for name in self.metrics.start_event_names:
            if not name:
                name = "start_event"
            clean = self._sanitize_single_name(name)
            if clean and clean not in sanitized:
                sanitized.append(clean)
        if not sanitized:
            sanitized = ["start_event"]
        return sanitized
    

if __name__ == '__main__': 
    processed_dir = os.path.join(os.path.dirname(__file__), '..', '..', 'bpmn', 'dataset', 'processed')
    bpmn_files = []
    # Files are now directly in processed_dir, not in subdirectories
    if os.path.exists(processed_dir):
        for file in os.listdir(processed_dir):
            if file.endswith('.bpmn'):
                bpmn_files.append(os.path.join(processed_dir, file))
    
    all_samples = []
    sample_id_global = 1
    
    for bpmn_file in bpmn_files:
        print(f"Processing {bpmn_file}")
        try:
            with open(bpmn_file, 'r', encoding='utf-8') as f:
                bpmn_content = f.read()
            
            simulator = SimpleBPMNSimulator(bpmn_content)
            # Generate multiple traces to distribute samples across
            traces_with_types = simulator.generate_traces_with_types(max_loops=0, max_traces=10)
            print(f"  Generated {len(traces_with_types)} traces")
            
            if not traces_with_types:
                print(f"  No traces generated, skipping...")
                continue
            
            # Model name is now just the filename since files are directly in processed/
            model_name = os.path.basename(bpmn_file)
            metrics = extract_bpmn_metrics(str(bpmn_file))
            
            generator = SampleGenerator(model_name, metrics)
            
            # Generate exactly 8 samples per model
            samples = generator.generate_samples_from_traces(traces_with_types, model_name)
            print(f"  Generated {len(samples)} samples")
            
            # Assign global sample IDs
            for sample in samples:
                sample['sample_id'] = sample_id_global
                sample_id_global += 1
            
            all_samples.extend(samples)
        except Exception as e:
            print(f"  Error processing {bpmn_file}: {e}")
            import traceback
            traceback.print_exc()
            continue
    
    output_path = os.path.join(os.path.dirname(os.path.dirname(__file__)), 'datasets', 'samples_leg_conf.csv')
    os.makedirs(os.path.dirname(output_path), exist_ok=True)
    
    if all_samples:
        with open(output_path, 'w', newline='') as f:
            writer = csv.DictWriter(f, fieldnames=all_samples[0].keys())
            writer.writeheader()
            for sample in all_samples:
                writer.writerow(sample)
        
        print(f"\n{'='*70}")
        print(f"Generated {len(all_samples)} samples total")
        print(f"Models processed: {len(all_samples) // 8}")
        print(f"Samples per model: 8 (4 legality + 4 conformance)")
        print(f"Written to: {output_path}")
        print(f"{'='*70}")
    else:
        print("No samples generated!")

