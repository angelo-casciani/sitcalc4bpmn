"""
BPMN Metrics Extractor

This module parses BPMN XML files and extracts:
- Structural metrics: #tasks, #exclusive gateways, #parallel gateways, #events, #pools, #subprocesses
- Elements for sample generation: task names, gateway conditions, data objects
"""

import xml.etree.ElementTree as ET
from dataclasses import dataclass, field
from typing import List, Dict, Set, Optional, Tuple
import re


@dataclass
class BPMNMetrics:
    """Container for BPMN model metrics."""
    num_tasks: int
    num_exclusive_gateways: int
    num_parallel_gateways: int
    num_events: int
    num_pools: int
    num_subprocesses: int
    
    # Elements for sample generation
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
        """Return total number of elements."""
        return (self.num_tasks + self.num_exclusive_gateways + 
                self.num_parallel_gateways + self.num_events + 
                self.num_pools + self.num_subprocesses)
    
    def to_dict(self) -> Dict:
        """Convert to dictionary for CSV export."""
        return {
            'num_tasks': self.num_tasks,
            'num_exclusive_gateways': self.num_exclusive_gateways,
            'num_parallel_gateways': self.num_parallel_gateways,
            'num_events': self.num_events,
            'num_pools': self.num_pools,
            'num_subprocesses': self.num_subprocesses,
            'total_elements': self.total_elements()
        }


class BPMNMetricsExtractor:
    """Extract metrics and elements from BPMN XML files."""
    
    # BPMN namespace
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


if __name__ == '__main__':
    import sys
    import json
    
    if len(sys.argv) < 2:
        print("Usage: python bpmn_metrics.py <bpmn_file_path>")
        sys.exit(1)
    
    bpmn_file = sys.argv[1]
    metrics = extract_bpmn_metrics(bpmn_file)
    
    print(f"\nBPMN Metrics for: {bpmn_file}")
    print("=" * 70)
    print(f"Tasks:                {metrics.num_tasks}")
    print(f"Exclusive Gateways:   {metrics.num_exclusive_gateways}")
    print(f"Parallel Gateways:    {metrics.num_parallel_gateways}")
    print(f"Events:               {metrics.num_events}")
    print(f"Pools:                {metrics.num_pools}")
    print(f"Subprocesses:         {metrics.num_subprocesses}")
    print(f"Total Elements:       {metrics.total_elements()}")
    print("\nTask Names:", metrics.task_names[:5], "..." if len(metrics.task_names) > 5 else "")
    print("Gateway Conditions:", metrics.gateway_conditions[:3], "..." if len(metrics.gateway_conditions) > 3 else "")
    print("Data Objects:", metrics.data_objects[:3], "..." if len(metrics.data_objects) > 3 else "")
    print("=" * 70)
