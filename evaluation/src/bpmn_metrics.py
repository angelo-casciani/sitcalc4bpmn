"""
BPMN Metrics Extractor

This module parses BPMN XML files and extracts:
- Structural metrics: #tasks, #exclusive gateways, #parallel gateways, #events, #pools, #subprocesses
- Elements for sample generation: task names, gateway conditions, data objects
"""

import xml.etree.ElementTree as ET
from dataclasses import dataclass, field
from typing import List, Dict, Optional
import re
import sys
import os


@dataclass
class BPMNMetrics:
    num_tasks: int
    num_exclusive_gateways: int
    num_parallel_gateways: int
    num_events: int
    num_pools: int
    num_subprocesses: int
    num_data_objects: int
    
    task_names: List[str]
    task_ids: List[str]
    gateway_conditions: List[str]
    gateway_ids: List[str]
    data_objects: List[str]
    data_object_ids: List[str]
    event_names: List[str]
    event_ids: List[str]
    start_event_names: List[str]  # Separate list for start events
    start_event_ids: List[str]
    first_task_after_start: Optional[str] = None  # First task in execution order
    parallel_branches: List[List[str]] = field(default_factory=list)  # List of task groups that can execute in parallel
    
    def total_elements(self) -> int:
        return (self.num_tasks + self.num_exclusive_gateways + 
                self.num_parallel_gateways + self.num_events + 
                self.num_pools + self.num_subprocesses)
    
    def to_dict(self) -> Dict:
        return {
            'num_tasks': self.num_tasks,
            'num_exclusive_gateways': self.num_exclusive_gateways,
            'num_parallel_gateways': self.num_parallel_gateways,
            'num_events': self.num_events,
            'num_pools': self.num_pools,
            'num_subprocesses': self.num_subprocesses,
            'num_data_objects': self.num_data_objects,
            'total_elements': self.total_elements()
        }


class BPMNMetricsExtractor:
    BPMN_NS = {'bpmn': 'http://www.omg.org/spec/BPMN/20100524/MODEL'}
    
    def __init__(self, bpmn_file_path: str):
        """Initialize the extractor with a BPMN file path.
        
        Args:
            bpmn_file_path: Path to the BPMN XML file
        """
        self.bpmn_file_path = bpmn_file_path
        self.tree = ET.parse(bpmn_file_path)
        self.root = self.tree.getroot()
        
        # Update namespace if not found (some files might not have namespace)
        if not self.root.tag.startswith('{'):
            self.BPMN_NS = {'bpmn': ''}
    
    def extract_metrics(self) -> BPMNMetrics:
        """Extract all metrics from the BPMN file.
        
        Returns:
            BPMNMetrics object containing all extracted metrics
        """
        # Count tasks (all types: manualTask, serviceTask, userTask, etc.)
        task_elements = []
        task_names = []
        task_ids = []
        
        for task_type in ['manualTask', 'serviceTask', 'userTask', 'scriptTask', 
                         'businessRuleTask', 'sendTask', 'receiveTask', 'task']:
            tasks = self._find_all_elements(task_type)
            task_elements.extend(tasks)
            for task in tasks:
                task_ids.append(self._get_attribute(task, 'id'))
                name = self._get_attribute(task, 'name')
                if name:
                    task_names.append(name)
        
        # Count gateways
        exclusive_gateways = self._find_all_elements('exclusiveGateway')
        parallel_gateways = self._find_all_elements('parallelGateway')
        
        gateway_ids = []
        gateway_conditions = []
        
        for gw in exclusive_gateways:
            gw_id = self._get_attribute(gw, 'id')
            gw_name = self._get_attribute(gw, 'name')
            gateway_ids.append(gw_id)
            if gw_name:
                gateway_conditions.append(gw_name)
        
        for gw in parallel_gateways:
            gateway_ids.append(self._get_attribute(gw, 'id'))
        
        # Count events (start, end, intermediate, etc.)
        event_elements = []
        event_names = []
        event_ids = []
        start_event_names = []
        start_event_ids = []
        
        start_event_counter = 0
        end_event_counter = 0
        
        for event_type in ['startEvent', 'endEvent', 'intermediateCatchEvent', 
                          'intermediateThrowEvent', 'boundaryEvent']:
            events = self._find_all_elements(event_type)
            event_elements.extend(events)
            for event in events:
                event_id = self._get_attribute(event, 'id')
                name = self._get_attribute(event, 'name')
                event_ids.append(event_id)
                
                # Use default name if no name provided
                if name:
                    event_names.append(name)
                else:
                    # Generate default name based on event type
                    if event_type == 'startEvent':
                        start_event_counter += 1
                        default_name = f'start_event' if start_event_counter == 1 else f'start_event_{start_event_counter}'
                        event_names.append(default_name)
                        name = default_name
                    elif event_type == 'endEvent':
                        end_event_counter += 1
                        default_name = f'end_event' if end_event_counter == 1 else f'end_event_{end_event_counter}'
                        event_names.append(default_name)
                        name = default_name
                    # For other event types without names, don't add to event_names
                
                # Separately track start events (with actual or default name)
                if event_type == 'startEvent':
                    start_event_ids.append(event_id)
                    if name:  # Will always be true now due to default naming
                        start_event_names.append(name)
        
        # Count pools (participants)
        pools = self._find_all_elements('participant')
        
        # Count subprocesses
        subprocesses = self._find_all_elements('subProcess')
        
        # Extract data objects
        data_objects = self._find_all_elements('dataObject')
        data_object_refs = self._find_all_elements('dataObjectReference')
        
        data_object_names = []
        data_object_ids_list = []
        
        for do in data_objects:
            do_id = self._get_attribute(do, 'id')
            do_name = self._get_attribute(do, 'name')
            data_object_ids_list.append(do_id)
            if do_name:
                data_object_names.append(do_name)
        
        for do_ref in data_object_refs:
            do_id = self._get_attribute(do_ref, 'id')
            do_name = self._get_attribute(do_ref, 'name')
            data_object_ids_list.append(do_id)
            if do_name:
                data_object_names.append(do_name)
        
        # Find the first task after the start event
        first_task_name = self._find_first_task_after_start(start_event_ids)
        
        # Extract parallel branches
        parallel_branches = self._extract_parallel_branches()
        
        return BPMNMetrics(
            num_tasks=len(task_elements),
            num_exclusive_gateways=len(exclusive_gateways),
            num_parallel_gateways=len(parallel_gateways),
            num_events=len(event_elements),
            num_pools=len(pools),
            num_subprocesses=len(subprocesses),
            num_data_objects=len(data_objects) + len(data_object_refs),
            task_names=task_names,
            task_ids=task_ids,
            gateway_conditions=gateway_conditions,
            gateway_ids=gateway_ids,
            data_objects=data_object_names,
            data_object_ids=data_object_ids_list,
            event_names=event_names,
            event_ids=event_ids,
            start_event_names=start_event_names,
            start_event_ids=start_event_ids,
            first_task_after_start=first_task_name,
            parallel_branches=parallel_branches
        )
    
    def _find_all_elements(self, tag_name: str) -> List[ET.Element]:
        """Find all elements with the given tag name.
        
        Args:
            tag_name: Name of the XML tag to search for
            
        Returns:
            List of matching elements
        """
        # Try with namespace first
        elements = self.root.findall(f'.//bpmn:{tag_name}', self.BPMN_NS)
        
        # If no namespace, try without
        if not elements and self.BPMN_NS.get('bpmn') == '':
            elements = self.root.findall(f'.//{tag_name}')
        
        # Also try with default namespace
        if not elements:
            default_ns = self._get_default_namespace()
            if default_ns:
                elements = self.root.findall(f'.//{{{default_ns}}}{tag_name}')
        
        return elements
    
    def _get_default_namespace(self) -> str:
        """Extract the default namespace from the root element.
        
        Returns:
            Default namespace URI or empty string
        """
        match = re.match(r'\{(.*?)\}', self.root.tag)
        return match.group(1) if match else ''
    
    def _get_attribute(self, element: ET.Element, attr_name: str) -> str:
        """Get attribute value from an element.
        
        Args:
            element: XML element
            attr_name: Attribute name
            
        Returns:
            Attribute value or empty string if not found
        """
        return element.get(attr_name, '')
    
    def _find_first_task_after_start(self, start_event_ids: List[str]) -> Optional[str]:
        """Find the first task that follows the start event in the control flow.
        
        Args:
            start_event_ids: List of start event IDs
            
        Returns:
            Name of the first task, or None if not found
        """
        if not start_event_ids:
            return None
        
        # Use the first start event
        start_id = start_event_ids[0]
        
        # Find the start event element
        start_elements = self._find_all_elements('startEvent')
        start_event = None
        for elem in start_elements:
            if self._get_attribute(elem, 'id') == start_id:
                start_event = elem
                break
        
        if not start_event:
            return None
        
        # Get outgoing sequence flows from start event
        outgoing_flows = start_event.findall('.//bpmn:outgoing', self.BPMN_NS)
        if not outgoing_flows:
            return None
        
        # Get the first outgoing flow
        flow_id = outgoing_flows[0].text
        
        # Find the target of this sequence flow
        all_flows = self._find_all_elements('sequenceFlow')
        target_id = None
        for flow in all_flows:
            if self._get_attribute(flow, 'id') == flow_id:
                target_id = self._get_attribute(flow, 'targetRef')
                break
        
        if not target_id:
            return None
        
        # Find the target element and get its name if it's a task
        for task_type in ['task', 'userTask', 'serviceTask', 'manualTask', 'scriptTask', 
                         'businessRuleTask', 'sendTask', 'receiveTask']:
            tasks = self._find_all_elements(task_type)
            for task in tasks:
                if self._get_attribute(task, 'id') == target_id:
                    task_name = self._get_attribute(task, 'name')
                    if task_name:
                        return task_name
        
        return None
    
    def _extract_parallel_branches(self) -> List[List[str]]:
        """Extract groups of tasks that can execute in parallel.
        
        Returns:
            List of task name lists, where each inner list contains tasks that can execute in parallel
        """
        parallel_groups = []
        
        # Find all parallel gateways
        parallel_gateways = self._find_all_elements('parallelGateway')
        sequence_flows = self._find_all_elements('sequenceFlow')
        
        # Build a map of sequence flows for quick lookup
        flow_map = {}
        for flow in sequence_flows:
            flow_id = self._get_attribute(flow, 'id')
            source = self._get_attribute(flow, 'sourceRef')
            target = self._get_attribute(flow, 'targetRef')
            flow_map[flow_id] = {'source': source, 'target': target}
        
        # Find diverging parallel gateways (splits)
        for gw in parallel_gateways:
            gw_id = self._get_attribute(gw, 'id')
            direction = self._get_attribute(gw, 'gatewayDirection')
            
            # Only process diverging (split) gateways
            if direction == 'Diverging':
                # Get outgoing flows from this gateway
                outgoing_elems = gw.findall('.//bpmn:outgoing', self.BPMN_NS)
                if not outgoing_elems:
                    continue
                
                # Track tasks in each branch
                branch_tasks = []
                
                for out_elem in outgoing_elems:
                    flow_id = out_elem.text
                    if not flow_id or flow_id not in flow_map:
                        continue
                    
                    # Follow this flow to find the first task
                    target_id = flow_map[flow_id]['target']
                    task_name = self._find_task_name_by_id(target_id)
                    
                    if task_name:
                        branch_tasks.append(task_name)
                
                # Only add if we found multiple parallel branches with tasks
                if len(branch_tasks) >= 2:
                    parallel_groups.append(branch_tasks)
        
        return parallel_groups
    
    def _find_task_name_by_id(self, task_id: str) -> Optional[str]:
        """Find a task name by its ID.
        
        Args:
            task_id: The ID of the task to find
            
        Returns:
            The task name, or None if not found or not a task
        """
        for task_type in ['task', 'userTask', 'serviceTask', 'manualTask', 'scriptTask', 
                         'businessRuleTask', 'sendTask', 'receiveTask']:
            tasks = self._find_all_elements(task_type)
            for task in tasks:
                if self._get_attribute(task, 'id') == task_id:
                    return self._get_attribute(task, 'name')
        return None


def extract_bpmn_metrics(bpmn_file_path: str) -> BPMNMetrics:
    """Convenience function to extract metrics from a BPMN file.
    
    Args:
        bpmn_file_path: Path to the BPMN XML file
        
    Returns:
        BPMNMetrics object
    """
    extractor = BPMNMetricsExtractor(bpmn_file_path)
    return extractor.extract_metrics()


def compute_metrics_for_dataset(dataset_dir: str, output_file: str, dataset_name: str):
    """Compute BPMN metrics for all .bpmn files in a directory.
    
    Args:
        dataset_dir: Directory containing BPMN files
        output_file: Path to output CSV file
        dataset_name: Name of the dataset for logging
    """
    import csv
    
    print(f"\n{'='*70}")
    print(f"Computing BPMN metrics for: {dataset_name}")
    print(f"Dataset directory: {dataset_dir}")
    print(f"{'='*70}")
    
    if not os.path.exists(dataset_dir):
        print(f"Error: Dataset directory not found: {dataset_dir}")
        return
    
    # Find all BPMN files
    bpmn_files = sorted([f for f in os.listdir(dataset_dir) if f.endswith('.bpmn')])
    
    if not bpmn_files:
        print(f"Warning: No BPMN files found in {dataset_dir}")
        return
    
    print(f"Found {len(bpmn_files)} BPMN files")
    
    # Collect metrics for all files
    all_metrics = []
    errors = []
    
    for i, bpmn_file in enumerate(bpmn_files, 1):
        bpmn_path = os.path.join(dataset_dir, bpmn_file)
        print(f"  [{i}/{len(bpmn_files)}] Processing: {bpmn_file}...", end=' ')
        
        try:
            metrics = extract_bpmn_metrics(bpmn_path)
            metrics_dict = metrics.to_dict()
            metrics_dict['model_name'] = bpmn_file
            all_metrics.append(metrics_dict)
            print("✓")
        except Exception as e:
            print(f"✗ Error: {e}")
            errors.append((bpmn_file, str(e)))
    
    # Save to CSV
    if all_metrics:
        os.makedirs(os.path.dirname(output_file), exist_ok=True)
        
        # Define fieldnames (model_name first, then metrics)
        fieldnames = ['model_name', 'num_tasks', 'num_exclusive_gateways', 
                     'num_parallel_gateways', 'num_events', 'num_pools', 
                     'num_subprocesses', 'num_data_objects', 'total_elements']
        
        with open(output_file, 'w', newline='') as f:
            writer = csv.DictWriter(f, fieldnames=fieldnames)
            writer.writeheader()
            writer.writerows(all_metrics)
        
        print(f"\n✓ Metrics saved to: {output_file}")
        print(f"  Successfully processed: {len(all_metrics)} models")
        
        if errors:
            print(f"  Failed to process: {len(errors)} models")
            for bpmn_file, error in errors:
                print(f"    - {bpmn_file}: {error}")
        
        # Generate summary
        generate_bpmn_metrics_summary(all_metrics, output_file, dataset_name)
    else:
        print("\nNo metrics collected")


def generate_bpmn_metrics_summary(metrics_list: list, csv_path: str, dataset_name: str):
    """Generate summary statistics for BPMN metrics.
    
    Args:
        metrics_list: List of metrics dictionaries
        csv_path: Path to the CSV file (used to determine output path)
        dataset_name: Name of the dataset for the report
    """
    import statistics
    from datetime import datetime
    
    if not metrics_list:
        return
    
    num_models = len(metrics_list)
    
    # Calculate statistics for each metric
    num_tasks_list = [m['num_tasks'] for m in metrics_list]
    num_exc_gw_list = [m['num_exclusive_gateways'] for m in metrics_list]
    num_par_gw_list = [m['num_parallel_gateways'] for m in metrics_list]
    num_events_list = [m['num_events'] for m in metrics_list]
    num_pools_list = [m['num_pools'] for m in metrics_list]
    num_subproc_list = [m['num_subprocesses'] for m in metrics_list]
    num_data_obj_list = [m['num_data_objects'] for m in metrics_list]
    total_elem_list = [m['total_elements'] for m in metrics_list]
    
    # Generate summary
    timestamp = datetime.now().strftime('%Y-%m-%d %H:%M:%S')
    
    summary = f"""
{'='*80}
BPMN STRUCTURAL METRICS SUMMARY - {dataset_name}
{'='*80}

Generated: {timestamp}
Dataset: {dataset_name}
Source File: {os.path.basename(csv_path)}

{'='*80}
DATASET OVERVIEW
{'='*80}

Total Models Analyzed: {num_models}
Total Tasks:           {sum(num_tasks_list)}
Total Gateways:        {sum(num_exc_gw_list) + sum(num_par_gw_list)}
Total Events:          {sum(num_events_list)}
Total Elements:        {sum(total_elem_list)}

{'='*80}
TASKS STATISTICS
{'='*80}

Average:        {statistics.mean(num_tasks_list):.1f} tasks/model
Median:         {statistics.median(num_tasks_list):.1f} tasks/model
Minimum:        {min(num_tasks_list)} tasks/model
Maximum:        {max(num_tasks_list)} tasks/model

{'='*80}
EXCLUSIVE GATEWAYS STATISTICS
{'='*80}

Average:        {statistics.mean(num_exc_gw_list):.1f} gateways/model
Median:         {statistics.median(num_exc_gw_list):.1f} gateways/model
Minimum:        {min(num_exc_gw_list)} gateways/model
Maximum:        {max(num_exc_gw_list)} gateways/model

{'='*80}
PARALLEL GATEWAYS STATISTICS
{'='*80}

Average:        {statistics.mean(num_par_gw_list):.1f} gateways/model
Median:         {statistics.median(num_par_gw_list):.1f} gateways/model
Minimum:        {min(num_par_gw_list)} gateways/model
Maximum:        {max(num_par_gw_list)} gateways/model

{'='*80}
EVENTS STATISTICS
{'='*80}

Average:        {statistics.mean(num_events_list):.1f} events/model
Median:         {statistics.median(num_events_list):.1f} events/model
Minimum:        {min(num_events_list)} events/model
Maximum:        {max(num_events_list)} events/model

{'='*80}
POOLS STATISTICS
{'='*80}

Average:        {statistics.mean(num_pools_list):.1f} pools/model
Median:         {statistics.median(num_pools_list):.1f} pools/model
Minimum:        {min(num_pools_list)} pools/model
Maximum:        {max(num_pools_list)} pools/model
Total:          {sum(num_pools_list)} pools

{'='*80}
SUBPROCESSES STATISTICS
{'='*80}

Average:        {statistics.mean(num_subproc_list):.1f} subprocesses/model
Median:         {statistics.median(num_subproc_list):.1f} subprocesses/model
Minimum:        {min(num_subproc_list)} subprocesses/model
Maximum:        {max(num_subproc_list)} subprocesses/model
Total:          {sum(num_subproc_list)} subprocesses

{'='*80}
DATA OBJECTS STATISTICS
{'='*80}

Average:        {statistics.mean(num_data_obj_list):.1f} data objects/model
Median:         {statistics.median(num_data_obj_list):.1f} data objects/model
Minimum:        {min(num_data_obj_list)} data objects/model
Maximum:        {max(num_data_obj_list)} data objects/model
Total:          {sum(num_data_obj_list)} data objects

{'='*80}
TOTAL ELEMENTS STATISTICS
{'='*80}

Average:        {statistics.mean(total_elem_list):.1f} elements/model
Median:         {statistics.median(total_elem_list):.1f} elements/model
Minimum:        {min(total_elem_list)} elements/model
Maximum:        {max(total_elem_list)} elements/model

{'='*80}
TOP 5 MOST COMPLEX MODELS (by total elements)
{'='*80}

"""
    
    sorted_by_complexity = sorted(metrics_list, key=lambda x: x['total_elements'], reverse=True)[:5]
    for i, m in enumerate(sorted_by_complexity, 1):
        summary += f"{i}. {m['model_name']}: {m['total_elements']} elements "
        summary += f"(tasks={m['num_tasks']}, gateways={m['num_exclusive_gateways']+m['num_parallel_gateways']}, events={m['num_events']})\n"
    
    summary += f"""
{'='*80}
TOP 5 MODELS WITH MOST TASKS
{'='*80}

"""
    
    sorted_by_tasks = sorted(metrics_list, key=lambda x: x['num_tasks'], reverse=True)[:5]
    for i, m in enumerate(sorted_by_tasks, 1):
        summary += f"{i}. {m['model_name']}: {m['num_tasks']} tasks\n"
    
    summary += f"""
{'='*80}
END OF SUMMARY
{'='*80}
"""
    
    # Write summary to file
    summary_path = csv_path.replace('.csv', '_summary.txt')
    with open(summary_path, 'w') as f:
        f.write(summary)
    
    print(f"  Summary saved to: {summary_path}")


if __name__ == '__main__':
    import argparse
    
    parser = argparse.ArgumentParser(description='Extract BPMN metrics from files')
    parser.add_argument('--file', type=str, help='Single BPMN file to analyze')
    parser.add_argument('--compute-all', action='store_true', 
                       help='Compute metrics for all datasets')
    
    args = parser.parse_args()
    
    if args.compute_all:
        # Compute metrics for both datasets
        script_dir = os.path.dirname(os.path.abspath(__file__))
        project_root = os.path.abspath(os.path.join(script_dir, '..', '..'))
        
        datasets = [
            {
                'name': 'Processed Dataset (Legality & Conformance)',
                'dir': os.path.join(project_root, 'bpmn', 'dataset', 'processed'),
                'output': os.path.join(project_root, 'evaluation', 'bpmn_metrics', 'bpmn_metrics_processed.csv')
            },
            {
                'name': 'Exams BPMN (Projection & Verification)',
                'dir': os.path.join(project_root, 'bpmn', 'exams-bpmn'),
                'output': os.path.join(project_root, 'evaluation', 'bpmn_metrics', 'bpmn_metrics_exams.csv')
            }
        ]
        
        for dataset in datasets:
            compute_metrics_for_dataset(
                dataset_dir=dataset['dir'],
                output_file=dataset['output'],
                dataset_name=dataset['name']
            )
        
        print(f"\n{'='*70}")
        print("BPMN metrics computation completed!")
        print(f"{'='*70}\n")
    
    elif args.file:
        # Single file analysis
        metrics = extract_bpmn_metrics(args.file)
        
        print(f"\nBPMN Metrics for: {args.file}")
        print("=" * 70)
        print(f"Tasks:                {metrics.num_tasks}")
        print(f"Exclusive Gateways:   {metrics.num_exclusive_gateways}")
        print(f"Parallel Gateways:    {metrics.num_parallel_gateways}")
        print(f"Events:               {metrics.num_events}")
        print(f"Pools:                {metrics.num_pools}")
        print(f"Subprocesses:         {metrics.num_subprocesses}")
        print(f"Data Objects:         {metrics.num_data_objects}")
        print(f"Total Elements:       {metrics.total_elements()}")
        print("\nTask Names:", metrics.task_names[:5], "..." if len(metrics.task_names) > 5 else "")
        print("Gateway Conditions:", metrics.gateway_conditions[:3], "..." if len(metrics.gateway_conditions) > 3 else "")
        print("Data Objects:", metrics.data_objects[:3], "..." if len(metrics.data_objects) > 3 else "")
        print("=" * 70)
    
    else:
        parser.print_help()
