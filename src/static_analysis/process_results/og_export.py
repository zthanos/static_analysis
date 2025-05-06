
import os
import json
from logger import logger 
import xml.etree.ElementTree as ET
import uuid


BASE_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), "../.."))
# Namespaces/constants
def _register_namespaces():
    ns = 'http://www.opengroup.org/xsd/archimate/3.0/'
    xsi = 'http://www.w3.org/2001/XMLSchema-instance'
    schema_location = (
        'http://www.opengroup.org/xsd/archimate/3.0/ '
        'http://www.opengroup.org/xsd/archimate/3.1/archimate3_Diagram.xsd'
    )
    ET.register_namespace('', ns)
    ET.register_namespace('xsi', xsi)
    return ns, xsi, schema_location
 
def process_json_data(json_file):
    with open(json_file) as f:
        data = json.load(f)
    return data

def generate_document(data):
    root, elements, relationships, organizations = init_model()
    
    program = data.get("program")
    flows = data.get('flow', [])
    methods_index = []
    relations_index = []
    
    application_elements = []
    relationships_elements = []
    comp_id = add_element(elements, 'ApplicationComponent', program)
    for flow in flows:
        entrypoint = flow.get("EntryPoint")
        
        # Components
        entrypoint_id = add_element(elements, 'ApplicationFunction', entrypoint)
        application_elements.append(comp_id)
        application_elements.append(entrypoint_id)
        # Relations
        rel1 = add_relationship(relationships, 'Composition', comp_id, entrypoint_id)
        relationships_elements.append(rel1)
        
        for index, analyzed in enumerate(flow.get('AnalyzedPaths', [])):
            call_to_systems, call_to_programs = process_external_systems(analyzed.get('CallsToExternalSystem', []))
            for item in call_to_programs:
                element_id = next((i for i in methods_index if item in i), None)
                if not element_id:
                    element_id = add_element(elements, 'ApplicationComponent', item)
                    methods_index.append({'id': element_id, 'element':item})
                relation = f'{entrypoint_id} -> {element_id}'
                if relation not in relations_index:
                    rel = add_relationship(relationships, 'Triggering', comp_id, element_id)
                    relations_index.append(f'{entrypoint_id} -> {element_id}')
                    relationships_elements.append(rel)

            for item in call_to_systems:         
                element_id = next((i['id'] for i in methods_index if item in i['element']), None)
                if not element_id:                      
                    element_id = add_element(elements, 'ApplicationComponent', item)
                    methods_index.append({'id': element_id, 'element':item})
                relation = f'{entrypoint_id} -> {element_id}'
                if relation not in relations_index:                    
                    rel = add_relationship(relationships, 'Triggering', entrypoint_id, element_id)          
                    relations_index.append(f'{entrypoint_id} -> {element_id}')
                    relationships_elements.append(rel)                     
    
    # Organizations
    add_organization(organizations, 'Application', application_elements)
    add_organization(organizations, 'Relations', relationships_elements)

    logger.info(to_string(root))



def process_external_systems(external_systems):
    cics = []
    programs = []
    
    for cmd in external_systems:
        splitted = cmd.split(' ')
        if len(splitted) > 1:
            if splitted[0] == 'XCTL':
                programs.append(splitted[1])
            else:
                cics.append(f'{splitted[0]}')
    return cics, programs

# ID generator
def _gen_id():
    return 'id-' + uuid.uuid4().hex

# Initialize a new model
def init_model(name='(new model)', identifier=None):
    ns, xsi, schema_location = _register_namespaces()
    root = ET.Element(
        ET.QName(ns, 'model'),
        {
            'identifier': identifier or _gen_id(),
            ET.QName(xsi, 'schemaLocation'): schema_location
        }
    )
    name_el = ET.SubElement(root, ET.QName(ns, 'name'), {'xml:lang': 'en'})
    name_el.text = name

    elements_el = ET.SubElement(root, ET.QName(ns, 'elements'))
    relationships_el = ET.SubElement(root, ET.QName(ns, 'relationships'))
    organizations_el = ET.SubElement(root, ET.QName(ns, 'organizations'))
    return root, elements_el, relationships_el, organizations_el

# Add an element to the model
def add_element(elements_el, xsi_type, name, identifier=None):
    ns = str(elements_el.tag).split('}')[0].strip('{')
    xsi = 'http://www.w3.org/2001/XMLSchema-instance'
    eid = identifier or _gen_id()
    el = ET.SubElement(
        elements_el,
        ET.QName(ns, 'element'),
        {
            'identifier': eid,
            ET.QName(xsi, 'type'): xsi_type
        }
    )
    name_el = ET.SubElement(el, ET.QName(ns, 'name'), {'xml:lang': 'en'})
    name_el.text = name
    return eid

# Add a relationship to the model
def add_relationship(relationships_el, xsi_type, source_id, target_id, identifier=None):
    ns = str(relationships_el.tag).split('}')[0].strip('{')
    xsi = 'http://www.w3.org/2001/XMLSchema-instance'
    rid = identifier or _gen_id()
    ET.SubElement(
        relationships_el,
        ET.QName(ns, 'relationship'),
        {
            'identifier': rid,
            'source': source_id,
            'target': target_id,
            ET.QName(xsi, 'type'): xsi_type
        }
    )
    return rid

# Add an organization group
def add_organization(organizations_el, label, identifier_refs):
    ns = str(organizations_el.tag).split('}')[0].strip('{')
    item_el = ET.SubElement(organizations_el, ET.QName(ns, 'item'))
    # <label>
    label_el = ET.SubElement(item_el, ET.QName(ns, 'label'), {'xml:lang': 'en'})
    label_el.text = label
    # nested <item identifierRef="..." />
    for ref in identifier_refs:
        ET.SubElement(item_el, ET.QName(ns, 'item'), {'identifierRef': ref})
    return item_el

# Pretty-print helper
def _indent(elem, level=0):
    indent_str = "\n" + level * "  "
    if len(elem):
        if not elem.text or not elem.text.strip():
            elem.text = indent_str + "  "
        for child in elem:
            _indent(child, level + 1)
        if not child.tail or not child.tail.strip():
            child.tail = indent_str
    if level and (not elem.tail or not elem.tail.strip()):
        elem.tail = indent_str

# Serialize to XML string
def to_string(root):
    _indent(root)
    return ET.tostring(root, encoding='utf-8', xml_declaration=True).decode('utf-8')
def validate_models(models_output):
    
    json_path = os.path.join(os.path.dirname(__file__),  "..\..\..\output\open_group", models_output)
    # Load XSD
    with open('models/archimate3_Model.xsd', 'rb') as f:
        schema_root = etree.XML(f.read())
    schema = etree.XMLSchema(schema_root)

    # Load XML
    xml_doc = etree.parse('path/to/your.xml')

    # Validate
    if schema.validate(xml_doc):
        print("XML is valid!")
    else:
        print("Invalid XML!")
        
def validate_views(models_output):
    
    json_path = os.path.join(os.path.dirname(__file__),  "..\..\..\output\open_group", models_output)
    # Load XSD
    with open('models/archimate3_View.xsd', 'rb') as f:
        schema_root = etree.XML(f.read())
    schema = etree.XMLSchema(schema_root)

    # Load XML
    xml_doc = etree.parse('path/to/your.xml')

    # Validate
    if schema.validate(xml_doc):
        print("XML is valid!")
    else:
        print("Invalid XML!")        
        
def validate_diagram(models_output):
    
    json_path = os.path.join(os.path.dirname(__file__),  "..\..\..\output\open_group", models_output)
    # Load XSD
    with open('models/archimate3_Diagram.xsd', 'rb') as f:
        schema_root = etree.XML(f.read())
    schema = etree.XMLSchema(schema_root)

    # Load XML
    xml_doc = etree.parse('path/to/your.xml')

    # Validate
    if schema.validate(xml_doc):
        print("XML is valid!")
    else:
        print("Invalid XML!")         

if __name__ == "__main__":
    # json_path = os.path.join(os.path.dirname(__file__),  "..\..\..\output\open_group\Analyzed_DOGETRAN.json")
    json_path = os.path.join(os.path.dirname(__file__),  "..\..\..\output\Analyzed_DOGETRAN.json")
 
    data = process_json_data(json_path)
    generate_document(data)
    print("Analysis Completed!")     










