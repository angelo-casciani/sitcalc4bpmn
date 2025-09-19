import xml.etree.ElementTree as ET


class Parser:
    def __init__(self, file_path):
        self.tree = ET.parse(file_path)
        self.root = self.tree.getroot()

        self.pools = []
        self.tasks = []
        self.start_events = []
        self.end_events = []
        self.throw_message_events = []
        self.catch_message_events = []
        self.flows = []
        self.data_objects = []
        self.data_associations = []
        self.xor_gateways = []
        self.event_based_gateways = []
        self.parallel_gateways = []

        self.elements_by_id = {}
        self.namespaces = {'bpmn': 'http://www.omg.org/spec/BPMN/20100524/MODEL'}

        self.populate_elements_by_id()
        self.parse()


    def populate_elements_by_id(self):
        for elem in self.root.findall('.//*', self.namespaces):
            elem_id = elem.get('id')
            if elem_id:
                self.elements_by_id[elem_id] = elem


    def parse(self):
        self.extract_pools()
        self.extract_tasks()
        self.extract_events()
        self.extract_message_events()
        self.extract_flows()
        self.extract_data_objects()
        self.extract_gateways()


    def extract_pools(self):
        for participant in self.root.findall('bpmn:collaboration/bpmn:participant', self.namespaces):
            self.pools.append(participant.get('name'))


    def extract_tasks(self):
        for task in self.root.findall('.//bpmn:task', self.namespaces):
            task_info = {
                'id': task.get('id'),
                'name': task.get('name')
            }
            self.tasks.append(task_info)
            self.elements_by_id[task_info['id']] = task_info


    def extract_events(self):
        for event in self.root.findall('.//bpmn:startEvent', self.namespaces):
            self.start_events.append(event.get('name'))
        for event in self.root.findall('.//bpmn:endEvent', self.namespaces):
            self.end_events.append(event.get('name'))


    def extract_message_events(self):
        for event in self.root.findall('.//bpmn:intermediateThrowEvent', self.namespaces):
            if event.find('bpmn:messageEventDefinition', self.namespaces) is not None:
                self.throw_message_events.append(event.get('name'))
        for event in self.root.findall('.//bpmn:intermediateCatchEvent', self.namespaces):
            if event.find('bpmn:messageEventDefinition', self.namespaces) is not None:
                self.catch_message_events.append(event.get('name'))


    def extract_flows(self):
        for flow in self.root.findall('.//bpmn:sequenceFlow', self.namespaces):
            source_id = flow.get('sourceRef')
            target_id = flow.get('targetRef')
            source_elem = self.elements_by_id.get(source_id)
            target_elem = self.elements_by_id.get(target_id)
            source_name = source_elem.get('name') if source_elem is not None and source_elem.get('name') else source_id
            target_name = target_elem.get('name') if target_elem is not None and target_elem.get('name') else target_id
            self.flows.append((source_name, target_name))


    def extract_data_objects(self):
        for data_object in self.root.findall('.//bpmn:dataObjectReference', self.namespaces):
            data_object_info = {
                'name': data_object.get('name'),
                'id': data_object.get('id')
            }
            self.data_objects.append(data_object_info)
            self.elements_by_id[data_object_info['id']] = data_object

        # Extract data input associations (data objects to activities)
        for association in self.root.findall('.//bpmn:dataInputAssociation', self.namespaces):
            parent_elem = next((elem for elem in self.root.iter() if association in list(elem)), None)
            target_id = parent_elem.get('id') if parent_elem is not None else None
            source_elems = association.findall('bpmn:sourceRef', self.namespaces)
            for source_elem in source_elems:
                source_id = source_elem.text
                self.data_associations.append({
                    'data_object': source_id,
                    'activity': target_id,
                    'type': 'input'
                })

        # Extract data output associations (activities to data objects)
        for association in self.root.findall('.//bpmn:dataOutputAssociation', self.namespaces):
            parent_elem = next((elem for elem in self.root.iter() if association in list(elem)), None)
            source_id = parent_elem.get('id') if parent_elem is not None else None
            target_elems = association.findall('bpmn:targetRef', self.namespaces)
            for target_elem in target_elems:
                target_id = target_elem.text
                self.data_associations.append({
                    'data_object': target_id,
                    'activity': source_id,
                    'type': 'output'
                })


    def extract_gateways(self):
        for gateway in self.root.findall('.//bpmn:exclusiveGateway', self.namespaces):
            gateway_info = {
                'id': gateway.get('id'),
                'name': gateway.get('name')
            }
            self.xor_gateways.append(gateway_info)
        for gateway in self.root.findall('.//bpmn:eventBasedGateway', self.namespaces):
            gateway_info = {
                'id': gateway.get('id'),
                'name': gateway.get('name')
            }
            self.event_based_gateways.append(gateway_info)
        for gateway in self.root.findall('.//bpmn:parallelGateway', self.namespaces):
            gateway_info = {
                'id': gateway.get('id'),
                'name': gateway.get('name')
            }
            self.parallel_gateways.append(gateway_info)
