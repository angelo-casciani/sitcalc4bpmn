import xml.etree.ElementTree as ET
from typing import List, Dict, Set, Tuple, FrozenSet
from collections import deque
import argparse
import sys
import time

# Set higher recursion depth for complex models
sys.setrecursionlimit(5000)

class SimpleBPMNSimulator:
    """
    A stateful, token-based BPMN simulator that generates all trace variants.
    Tokens are on the SEQUENCE FLOWS.
    
    Handles:
    - Sequence Flows
    - Exclusive Gateways (XOR)
    - Parallel Gateways (AND)
    - Simple Loops (with loop limit)
    """
    
    def __init__(self, bpmn_content: str):
        self.root = ET.fromstring(bpmn_content)
        self.ns = {'bpmn': 'http://www.omg.org/spec/BPMN/20100524/MODEL'}
        
        self.elements = {}  # element_id -> (type, name)
        self.flows = {}  # source_element_id -> [flow_id_1, flow_id_2, ...]
        self.flow_targets = {} # flow_id -> target_element_id
        self.incoming_flows = {}  # target_element_id -> [flow_id_1, flow_id_2, ...]
        self.gateway_directions = {} # gateway_id -> 'Diverging' or 'Converging'
        self.flow_names = {}  # flow_id -> flow_name (for XOR branch conditions)
        self.xor_gateway_flows = {}  # gateway_id -> {flow_id: flow_name}
        
        self.start_event_id = None
        
        self._parse_bpmn()
        self._determine_gateway_directions()
        self._identify_xor_branches()

    def _parse_bpmn(self):
        """Parse all relevant BPMN elements into simple lookups."""
        
        all_element_types = [
            'task', 'userTask', 'serviceTask', 'manualTask', 'scriptTask', 
            'sendTask', 'receiveTask', 'exclusiveGateway', 'parallelGateway', 
            'inclusiveGateway', 'intermediateCatchEvent', 'intermediateThrowEvent',
            'startEvent', 'endEvent'
        ]

        for elem_type in all_element_types:
            for elem in self.root.findall(f'.//bpmn:{elem_type}', self.ns):
                elem_id = elem.get('id')
                elem_name = elem.get('name', '')
                self.elements[elem_id] = (elem_type, elem_name)
                
                if elem_type == 'startEvent':
                    if not self.start_event_id:
                        self.start_event_id = elem_id
        
        for flow in self.root.findall('.//bpmn:sequenceFlow', self.ns):
            flow_id = flow.get('id')
            source = flow.get('sourceRef')
            target = flow.get('targetRef')
            flow_name = flow.get('name', '')
            
            if not all([flow_id, source, target]):
                continue # Skip invalid flows

            if source not in self.flows:
                self.flows[source] = []
            self.flows[source].append(flow_id)
            
            self.flow_targets[flow_id] = target
            self.flow_names[flow_id] = flow_name
            
            if target not in self.incoming_flows:
                self.incoming_flows[target] = []
            self.incoming_flows[target].append(flow_id)

    def _determine_gateway_directions(self):
        """
        Correctly determine gateway direction based on flow counts.
        """
        for elem_id, (elem_type, _) in self.elements.items():
            if 'Gateway' in elem_type:
                incoming = len(self.incoming_flows.get(elem_id, []))
                outgoing = len(self.flows.get(elem_id, []))
                
                if incoming > 1 and outgoing == 1:
                    self.gateway_directions[elem_id] = 'Converging'
                elif incoming == 1 and outgoing > 1:
                    self.gateway_directions[elem_id] = 'Diverging'
                elif incoming > 1 and outgoing > 1:
                    self.gateway_directions[elem_id] = 'Complex'
                else:
                    self.gateway_directions[elem_id] = 'Unspecified'
    
    def _identify_xor_branches(self):
        """
        Identify exclusive gateways and map their outgoing flows to branch conditions.
        Also identify which activities feed into diverging XOR gateways.
        """
        # Map: activity_elem_id -> list of (flow_id, condition_value) for XOR branches
        self.activity_to_xor_branches = {}
        
        for elem_id, (elem_type, _) in self.elements.items():
            if elem_type == 'exclusiveGateway' and self.gateway_directions.get(elem_id) == 'Diverging':
                # This is a diverging XOR gateway
                outgoing_flows = self.flows.get(elem_id, [])
                self.xor_gateway_flows[elem_id] = {}
                
                # Find what feeds into this XOR gateway
                incoming_flows = self.incoming_flows.get(elem_id, [])
                for in_flow_id in incoming_flows:
                    # Find the source element
                    for source_elem_id, flow_list in self.flows.items():
                        if in_flow_id in flow_list:
                            # This source element feeds into the XOR gateway
                            # Map the XOR branches to this element
                            branches = []
                            for flow_id in outgoing_flows:
                                flow_name = self.flow_names.get(flow_id, '')
                                # Sanitize the flow name to extract value (yes/no/etc)
                                value = self._extract_branch_value(flow_name)
                                branches.append((flow_id, value))
                                self.xor_gateway_flows[elem_id][flow_id] = value
                            
                            if source_elem_id not in self.activity_to_xor_branches:
                                self.activity_to_xor_branches[source_elem_id] = []
                            self.activity_to_xor_branches[source_elem_id].extend(branches)
    
    def _extract_branch_value(self, flow_name: str) -> str:
        """Extract the branch value from a flow name (e.g., 'yes', 'no')."""
        if not flow_name:
            return ''
        # Common patterns: "yes", "no", "> C", "< C", etc.
        flow_lower = flow_name.lower().strip()
        if flow_lower in ['yes', 'no']:
            return flow_lower
        # Try to extract from longer names
        if 'yes' in flow_lower:
            return 'yes'
        if 'no' in flow_lower:
            return 'no'
        # Return original if no pattern matched
        return flow_name.strip()

    def _get_element_name(self, elem_id: str) -> str:
        """Helper to get a display name for an element."""
        if elem_id in self.elements:
            elem_type, elem_name = self.elements[elem_id]
            
            # Only add Tasks and named Events to the trace
            # FIX: Check for 'task' in lowercased type
            if 'task' in elem_type.lower():
                return elem_name or elem_id
            if 'Event' in elem_type:
                if elem_type == 'startEvent':
                    return elem_name or "Start Event"
                elif elem_name:
                    return elem_name
        return None
    
    def _get_element_info(self, elem_id: str) -> tuple:
        """Get both name and type for an element.
        
        Returns:
            tuple: (name, type) or (None, None) if element should not be in trace
        """
        if elem_id in self.elements:
            elem_type, elem_name = self.elements[elem_id]
            
            # Only add Tasks and named Events to the trace
            if 'task' in elem_type.lower():
                return (elem_name or elem_id, elem_type)
            if 'Event' in elem_type:
                if elem_type == 'startEvent':
                    return (elem_name or "Start Event", elem_type)
                elif elem_name:
                    return (elem_name, elem_type)
        return (None, None)

    def generate_traces_with_types(self, max_loops: int = 1, max_traces: int = 100, timeout: int = 5) -> List[List[Tuple]]:
        """
        Generates execution traces with element type and XOR branch information.
        
        Returns:
            A list of traces, where each trace is a list of tuples:
            (activity_name, element_type, xor_value_or_none)
        """
        # First generate the simple traces
        simple_traces = self.generate_traces(max_loops, max_traces, timeout)
        
        # Build a mapping from activity names to element IDs and types
        name_to_elem_info = {}  # name -> (elem_id, elem_type)
        for elem_id, (elem_type, elem_name) in self.elements.items():
            if 'task' in elem_type.lower() or 'Event' in elem_type:
                name_to_use = elem_name or elem_id
                if elem_type == 'startEvent':
                    name_to_use = elem_name or "Start Event"
                if name_to_use:
                    name_to_elem_info[name_to_use] = (elem_id, elem_type)
        
        # Convert simple traces to traces with type and XOR info
        typed_traces = []
        for trace in simple_traces:
            typed_trace = []
            for i, activity_name in enumerate(trace):
                elem_id, elem_type = name_to_elem_info.get(activity_name, (None, 'unknown'))
                
                # Check if this activity feeds into an XOR gateway
                xor_value = None
                if elem_id and elem_id in self.activity_to_xor_branches:
                    # This activity precedes an XOR gateway
                    # Determine which branch was taken by looking at the next activity
                    if i + 1 < len(trace):
                        next_activity = trace[i + 1]
                        next_elem_id, _ = name_to_elem_info.get(next_activity, (None, None))
                        
                        if next_elem_id:
                            # Find which XOR branch leads to this next activity
                            xor_value = self._determine_xor_branch(elem_id, next_elem_id)
                
                typed_trace.append((activity_name, elem_type, xor_value))
            
            typed_traces.append(typed_trace)
        
        return typed_traces
    
    def _determine_xor_branch(self, source_elem_id: str, target_elem_id: str) -> str:
        """
        Determine which XOR branch value was taken from source to target.
        
        Args:
            source_elem_id: The activity that feeds into XOR gateway
            target_elem_id: The next activity in the trace
            
        Returns:
            The branch value (e.g., 'yes', 'no') or None
        """
        # Find the XOR gateway that source feeds into
        outgoing_flows = self.flows.get(source_elem_id, [])
        for flow_id in outgoing_flows:
            flow_target = self.flow_targets.get(flow_id)
            if flow_target:
                elem_type, _ = self.elements.get(flow_target, (None, None))
                if elem_type == 'exclusiveGateway' and self.gateway_directions.get(flow_target) == 'Diverging':
                    # This is the XOR gateway
                    # Find which outgoing flow from the gateway leads to target
                    xor_outflows = self.flows.get(flow_target, [])
                    for xor_flow_id in xor_outflows:
                        # Check if this flow eventually leads to target_elem_id
                        if self._flow_leads_to(xor_flow_id, target_elem_id):
                            return self.xor_gateway_flows.get(flow_target, {}).get(xor_flow_id, '')
        return None
    
    def _flow_leads_to(self, flow_id: str, target_elem_id: str, max_depth: int = 10) -> bool:
        """
        Check if a flow eventually leads to a target element (within max_depth steps).
        """
        if max_depth <= 0:
            return False
        
        immediate_target = self.flow_targets.get(flow_id)
        if immediate_target == target_elem_id:
            return True
        
        # Check through gateways (skip over converging gateways and other non-activity elements)
        if immediate_target:
            elem_type, _ = self.elements.get(immediate_target, (None, None))
            # If it's a gateway or other non-activity, continue following flows
            if elem_type and 'Gateway' in elem_type:
                outgoing = self.flows.get(immediate_target, [])
                for out_flow in outgoing:
                    if self._flow_leads_to(out_flow, target_elem_id, max_depth - 1):
                        return True
        
        return False

    def generate_traces(self, max_loops: int = 1, max_traces: int = 100, timeout: int = 5) -> Set[Tuple[str, ...]]:
        """
        Generates all execution traces using a state-space search (BFS).
        A "state" is defined by the set of active tokens and the trace so far.
        
        Args:
            max_loops: Max times a loop (re-visiting an element) is allowed.
        
        Returns:
            A set of unique traces (each trace is a tuple of activity names).
        """
        if not self.start_event_id:
            print("Error: No start event found.", file=sys.stderr)
            return set()
        
        completed_traces = set()
        
        # The queue holds states to explore:
        # (current_trace_tuple, active_tokens_frozenset, visited_counts_dict)
        initial_trace = tuple()
        start_name = self._get_element_name(self.start_event_id)
        if start_name:
            initial_trace = (start_name,)
            
        # Initial tokens are the FLOW IDs leaving the start event
        initial_tokens = frozenset(self.flows.get(self.start_event_id, []))
        initial_visited = {self.start_event_id: 1} # Count start event visit
        
        queue = deque([(initial_trace, initial_tokens, initial_visited)])
        
        # memoization: stores visited states to avoid redundant work
        memo = set() 
        
        max_queue_size = 50000

        start_time = time.time()
        
        while queue and len(completed_traces) < max_traces:
            if time.time() - start_time > timeout:
                print(f"Timeout reached after {timeout} seconds.", file=sys.stderr)
                break
                
            if len(queue) > max_queue_size:
                print(f"Warning: Simulation space is very large (>{max_queue_size} states). Stopping early.", file=sys.stderr)
                break
                
            current_trace, current_tokens, visited_counts = queue.popleft()
            
            state_key = (current_tokens, frozenset(visited_counts.items()))
            if state_key in memo:
                continue
            memo.add(state_key)
            
            # If no tokens are left, this trace is complete
            if not current_tokens:
                if current_trace: # Don't add empty traces
                    completed_traces.add(current_trace)
                    if len(completed_traces) >= max_traces:
                        break
                continue
            
            # --- This is the core logic ---
            # 1. Find AND-Joins that can fire (consume multiple tokens)
            
            firable_and_joins = {} # {join_id: [consumed_token_flow_ids]}
            tokens_to_consume = set()
            
            # Find all target elements for current tokens
            target_elements = set(self.flow_targets.get(t) for t in current_tokens if t in self.flow_targets)

            for elem_id in target_elements:
                if elem_id not in self.elements: continue
                elem_type, _ = self.elements[elem_id]
                
                # Check if this element is an AND-Join
                if 'parallelGateway' in elem_type and self.gateway_directions.get(elem_id) == 'Converging':
                    incoming_flow_ids = self.incoming_flows.get(elem_id, [])
                    # Check if ALL required tokens (flow_ids) are present
                    if all(flow_id in current_tokens for flow_id in incoming_flow_ids):
                        firable_and_joins[elem_id] = incoming_flow_ids
                        tokens_to_consume.update(incoming_flow_ids)
            
            # 2. Process the AND-Joins that fired
            next_trace = current_trace
            next_tokens = set(current_tokens)
            next_visited = dict(visited_counts)
            
            if firable_and_joins:
                next_tokens.difference_update(tokens_to_consume)
                for join_id in firable_and_joins:
                    # Add outgoing flows from the join
                    next_tokens.update(self.flows.get(join_id, []))
            
            # 3. Process all remaining single-token-consuming elements
            self._fire_single_tokens(next_trace, frozenset(next_tokens), next_visited, queue, completed_traces, max_loops, start_time, timeout)

        return completed_traces

    def _fire_single_tokens(self, 
                            trace: Tuple[str, ...], 
                            tokens: FrozenSet[str], 
                            visited: Dict[str, int], 
                            queue: deque, 
                            completed_traces: set,
                            max_loops: int,
                            start_time: float,
                            timeout: int):
        """
        Recursively explores all possible "next steps" for a set of single tokens.
        This is what generates the parallel interleavings.
        """
        
        if time.time() - start_time > timeout:
            return
        
        # Base case: no more single tokens to fire, add this state to the queue.
        # This state might just contain tokens waiting at an AND-join.
        if not tokens:
            if not trace: # Avoid adding empty traces
                return
            # This state has no more active tokens.
            # Add it to the main queue for the main loop to check
            # (it will be marked "complete" if tokens is empty)
            queue.append((trace, tokens, visited))
            return
            
        has_fired = False
        for token_id in tokens:
            
            # Get the target element this token is pointing to
            elem_id = self.flow_targets.get(token_id)
            if not elem_id or elem_id not in self.elements:
                continue # Token is dead or points to nothing
                
            # Check for loops
            visit_count = visited.get(elem_id, 0)
            # FIX: Check > max_loops to allow loop to run 'max_loops' times.
            if visit_count > max_loops:
                continue # This path is stopped due to loop limit
            
            elem_type, elem_name = self.elements[elem_id]
            
            # Check for and-joins, they are handled by the caller (main loop)
            if 'parallelGateway' in elem_type and self.gateway_directions.get(elem_id) == 'Converging':
                continue
                
            has_fired = True
            
            # --- This token can fire ---
            
            new_trace = tuple(trace)
            activity_name = self._get_element_name(elem_id)
            if activity_name:
                new_trace += (activity_name,)

            new_visited = dict(visited)
            # FIX: Increment visit count correctly.
            new_visited[elem_id] = visit_count + 1
            
            # Get the set of tokens *after* this one has been processed
            remaining_tokens = tokens - {token_id}
            
            # Add outgoing flows from this element as new tokens
            new_tokens_from_fire = self.flows.get(elem_id, [])
            
            # Handle different gateway splits
            if 'exclusiveGateway' in elem_type and self.gateway_directions.get(elem_id) == 'Diverging':
                # Create a new simulation branch for EACH outgoing path
                for out_flow_id in new_tokens_from_fire:
                    branch_tokens = remaining_tokens.union({out_flow_id})
                    self._fire_single_tokens(new_trace, branch_tokens, new_visited, queue, completed_traces, max_loops, start_time, timeout)
            
            elif 'parallelGateway' in elem_type and self.gateway_directions.get(elem_id) == 'Diverging':
                # Add ALL outgoing paths to the token set
                all_branch_tokens = remaining_tokens.union(new_tokens_from_fire)
                self._fire_single_tokens(new_trace, all_branch_tokens, new_visited, queue, completed_traces, max_loops, start_time, timeout)

            elif 'endEvent' in elem_type:
                # End event: consumes token, adds no new ones
                self._fire_single_tokens(new_trace, remaining_tokens, new_visited, queue, completed_traces, max_loops, start_time, timeout)

            else: 
                # Default: (Task, XOR-Join, Inclusive-Gateway, etc.)
                # Consumes one token, adds all outgoing tokens
                next_tokens = remaining_tokens.union(new_tokens_from_fire)
                self._fire_single_tokens(new_trace, next_tokens, new_visited, queue, completed_traces, max_loops, start_time, timeout)

        # If no tokens could be fired (e.g., all are at AND-joins or max loops)
        # then this is a "wait state". Add it to the queue for the next big loop.
        if not has_fired:
            queue.append((trace, tokens, visited))

# Example usage
if __name__ == "__main__":
    
    parser = argparse.ArgumentParser(description='Generate execution traces from BPMN models')
    parser.add_argument('--bpmn_file', type=str, required=True, help='Path to the BPMN file')
    parser.add_argument('--max_loops', type=int, default=1, help='Maximum loop iterations (e.g., 0=no loops, 1=run loop once)')
    parser.add_argument('--timeout', type=int, default=5, help='Timeout in seconds for trace generation')
    
    args = parser.parse_args()
    
    try:
        with open(args.bpmn_file, 'r', encoding='utf-8') as f:
            bpmn_content = f.read()
    except FileNotFoundError:
        print(f"Error: File not found at {args.bpmn_file}", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"Error reading file: {e}", file=sys.stderr)
        sys.exit(1)
    
    simulator = SimpleBPMNSimulator(bpmn_content)
    
    print(f"Simulating '{args.bpmn_file}' with max_loops={args.max_loops}...\n")
    
    # Generate traces
    traces = simulator.generate_traces(max_loops=args.max_loops, timeout=args.timeout)
    
    print(f"\n{'='*60}")
    print(f"Generated {len(traces)} unique trace variants:")
    print('='*60)
    
    if not traces:
        print("No traces were generated. Check model or loop settings.")
    
    # Sort traces for consistent output
    sorted_traces = sorted(list(traces), key=lambda t: (len(t), t))
    
    for i, trace in enumerate(sorted_traces, 1):
        print(f"{i}. {' -> '.join(trace)}")

