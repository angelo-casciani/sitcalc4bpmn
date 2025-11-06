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
    
    def _convert_to_indigolog_actions(self, trace_names, use_full_trace=False):
        """Convert trace names to IndiGolog format with start/end pairs.
        
        Args:
            trace_names: List of activity names from trace
            use_full_trace: If True, use full trace; if False, use half-trace (for legality checks)
        
        Returns:
            List of IndiGolog-formatted action strings
        """
        import math
        
        if not trace_names:
            return ['start_event(1)']
        
        # For conformance: use full trace
        # For legality: use half trace
        if use_full_trace:
            selected_trace = trace_names
        else:
            half_len = math.ceil(len(trace_names) / 2)
            selected_trace = trace_names[:half_len]
        
        indigolog_actions = []
        
        # The first element should be the start event (exogenous action without start/end)
        # It's typically named something like 'unemployed', 'job_needed', etc.
        # Format: event_name(1)
        if selected_trace:
            indigolog_actions.append(f'{selected_trace[0]}(1)')
            remaining = selected_trace[1:]
        else:
            remaining = []
        
        # Convert each activity to start/end pair
        for activity in remaining:
            # Check if this is an event or special activity (typically no start/end)
            # Events include: end events, intermediate events, boundary events, etc.
            # Heuristics: contains "end", "finish", "complete", "received", "finished", etc.
            # Or is an adjective/state like "unemployed"
            is_event = (
                'end' in activity.lower() or 
                'finish' in activity.lower() or 
                'complete' in activity.lower() or
                'received' in activity.lower() or
                'sent' in activity.lower() or
                activity.lower().startswith('still_') or
                activity.lower() in ['unemployed', 'job_needed', 'job_application_started']
            )
            
            if is_event:
                # Events are just: event_name(1)
                indigolog_actions.append(f'{activity}(1)')
            else:
                # Regular activities: activity(start,1) then activity(end,1)
                indigolog_actions.append(f'{activity}(start,1)')
                indigolog_actions.append(f'{activity}(end,1)')
        
        return indigolog_actions
    
    def generate_samples_from_traces(self, traces: List[Tuple[str, ...]], model_name: str) -> List[Dict]:
        """Generate legality and conformance samples from traces.
        
        For each trace, generates:
        - Legality TRUE sample (original valid trace in IndiGolog format)
        - Conformance TRUE sample (original valid trace in IndiGolog format)
        - Legality FALSE sample (if possible: swapped activities after start event)
        - Conformance FALSE sample (if possible: reversed trace)
        
        Args:
            traces: List of trace tuples (each tuple is a sequence of activity names)
            model_name: Name of the model
            
        Returns:
            List of generated samples with IndiGolog-formatted actions
        """
        samples = []
        sample_id = 1
        
        for trace in traces:
            if not trace:
                continue
            
            # Use sanitized trace names
            trace_names = [self._sanitize_single_name(name) for name in trace]
            
            # Convert to IndiGolog format
            # Legality: use half-trace (just checking executability)
            # Conformance: use full trace (checking complete execution path)
            indigolog_actions_half = self._convert_to_indigolog_actions(trace_names, use_full_trace=False)
            indigolog_actions_full = self._convert_to_indigolog_actions(trace_names, use_full_trace=True)
            
            # TRUE samples - original valid traces
            # Legality TRUE - use half-trace
            samples.append({
                'task_type': "legality",
                'sample_id': sample_id,
                'actions': ';'.join(indigolog_actions_half) if indigolog_actions_half else '',
                'expected_result': True,
                'model_name': model_name
            })
            sample_id += 1
            
            # Conformance TRUE - use full trace
            samples.append({
                'task_type': "conformance",
                'sample_id': sample_id,
                'actions': ';'.join(indigolog_actions_full) if indigolog_actions_full else '',
                'expected_result': True,
                'model_name': model_name
            })
            sample_id += 1
            
            # FALSE samples - modified traces
            # Need at least 3 activities in original trace to create meaningful swaps
            if len(trace_names) >= 3:
                # Swap activities at positions 1 and 2 (keeping start event at position 0)
                swapped_trace = list(trace_names)
                swapped_trace[1], swapped_trace[2] = swapped_trace[2], swapped_trace[1]
                swapped_indigolog = self._convert_to_indigolog_actions(swapped_trace)
                
                # Legality FALSE
                samples.append({
                    'task_type': "legality",
                    'sample_id': sample_id,
                    'actions': ';'.join(swapped_indigolog) if swapped_indigolog else '',
                    'expected_result': False,
                    'model_name': model_name
                })
                sample_id += 1
                
                # Conformance FALSE - reverse the full trace IndiGolog actions
                reversed_indigolog = indigolog_actions_full[::-1]
                samples.append({
                    'task_type': "conformance",
                    'sample_id': sample_id,
                    'actions': ';'.join(reversed_indigolog) if reversed_indigolog else '',
                    'expected_result': False,
                    'model_name': model_name
                })
                sample_id += 1
        
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
            traces = simulator.generate_traces(max_loops=0, max_traces=10)
            print(f"Generated {len(traces)} traces")
            selected_traces = list(traces)[:4]
            print(f"Using {len(selected_traces)} traces")
            
            # Model name is now just the filename since files are directly in processed/
            model_name = os.path.basename(bpmn_file)
            metrics = extract_bpmn_metrics(str(bpmn_file))
            
            generator = SampleGenerator(model_name, metrics)
            start_events = generator._get_sanitized_start_events()
            start_event = start_events[0] if start_events else None
            
            samples = generator.generate_samples_from_traces(selected_traces, model_name)
            for sample in samples:
                sample['sample_id'] = sample_id_global
                sample_id_global += 1
            
            all_samples.extend(samples)
        except Exception as e:
            print(f"Error processing {bpmn_file}: {e}")
            continue
    
    output_path = os.path.join(os.path.dirname(os.path.dirname(__file__)), 'datasets', 'all_samples.csv')
    os.makedirs(os.path.dirname(output_path), exist_ok=True)
    with open(output_path, 'w', newline='') as f:
        writer = csv.DictWriter(f, fieldnames=all_samples[0].keys())
        writer.writeheader()
        for sample in all_samples:
            writer.writerow(sample)
    
    print(f"Generated {len(all_samples)} samples, written to {output_path}")
