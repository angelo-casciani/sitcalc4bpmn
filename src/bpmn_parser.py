import xml.etree.ElementTree as ET
import re


class BPMNParser:
    def __init__(self, filepath):
        self.filepath = filepath
        self.tree = ET.parse(filepath)
        self.root = self.tree.getroot()
        self.ns = {'bpmn': 'http://www.omg.org/spec/BPMN/20100524/MODEL'}
        self.processes = {}
        self.participants = {}
        self.message_flows = {}
        self.data_objects = {}
        self._parse()

    def _clean_tag(self, tag):
        return re.sub(r'\{.*?\}', '', tag)

    def _get_element_data_associations(self, elem_xml):
        inputs, outputs = [], []
        for assoc in elem_xml.findall('bpmn:dataInputAssociation', self.ns):
            source_ref = assoc.find('bpmn:sourceRef', self.ns)
            if source_ref is not None: inputs.append(source_ref.text)
        for assoc in elem_xml.findall('bpmn:dataOutputAssociation', self.ns):
            target_ref = assoc.find('bpmn:targetRef', self.ns)
            if target_ref is not None: outputs.append(target_ref.text)
        return inputs, outputs

    def _parse_elements_recursively(self, parent_xml, element_dict):
        contained_elements = []
        for elem_xml in parent_xml:
            tag = self._clean_tag(elem_xml.tag)
            elem_id = elem_xml.get('id')
            if not elem_id or 'sequenceFlow' in tag or 'lane' in tag:
                continue
            
            contained_elements.append(elem_id)
            if tag == 'dataObjectReference':
                self.data_objects[elem_id] = {'name': elem_xml.get('name')}
                continue

            data_inputs, data_outputs = self._get_element_data_associations(elem_xml)
            
            element_info = {
                'id': elem_id,
                'name': elem_xml.get('name', ''),
                'type': tag,
                'incoming': [inc.text for inc in elem_xml.findall('bpmn:incoming', self.ns)],
                'outgoing': [out.text for out in elem_xml.findall('bpmn:outgoing', self.ns)],
                'default': elem_xml.get('default'),
                'data_inputs': data_inputs,
                'data_outputs': data_outputs,
                'is_event_subprocess': elem_xml.get('triggeredByEvent') == 'true',
                'is_terminate': bool(elem_xml.find('bpmn:terminateEventDefinition', self.ns) is not None),
                'parent_id': parent_xml.get('id')
            }

            if tag == 'subProcess': # Recursively parse children of the sub-process
                element_info['contained_elements'] = self._parse_elements_recursively(elem_xml, element_dict)

            element_dict[elem_id] = element_info
        return contained_elements

    def _parse(self):
        for participant in self.root.findall('.//bpmn:participant', self.ns):
            p_id = participant.get('id')
            self.participants[p_id] = {'name': participant.get('name'), 'processRef': participant.get('processRef')}

        for msg_flow in self.root.findall('.//bpmn:messageFlow', self.ns):
            self.message_flows[msg_flow.get('sourceRef')] = msg_flow.get('targetRef')

        for process_xml in self.root.findall('.//bpmn:process', self.ns):
            process_id = process_xml.get('id')
            elements = {}
            self._parse_elements_recursively(process_xml, elements)
            
            sequence_flows = {
                sf.get('id'): {'source': sf.get('sourceRef'), 'target': sf.get('targetRef')}
                for sf in process_xml.findall('.//bpmn:sequenceFlow', self.ns)
            }
            
            self.processes[process_id] = {
                'id': process_id,
                'elements': elements,
                'sequence_flows': sequence_flows
            }
            
    def get_participant_by_process_id(self, process_id):
        for p_info in self.participants.values():
            if p_info['processRef'] == process_id: return p_info
        return None