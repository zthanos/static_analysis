
import os
import json
from logger import logger 
from plantuml import PlantUML
import sys

class SwimLanePallette:
    """Class to hold the colors for the swimlane diagram."""
    # Define colors as class attributes
    SKY_BLUE = "#3498DB"
    SUNSET_RED = "#E74C3C"
    GOLDEN_YELLOW = "#F1C40F"
    EMERALD_GREEN = "#2ECC71"
    
EXTERNAL_CALL="|External|"
OTHER_COBOL_PROGRAMS="|Other COBOL Programs|"

BASE_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), "../.."))
def process_json_data(json_file):
    with open(json_file) as f:
        data = json.load(f)
    return data
def generate_document(data):
    program = data.get("program")
    flows = data.get('flow', [])
    entry_points, external_systems = extract_calls(data)
    for flow in flows:
        plantuml_code = []
        sl_diagram = []
      
        plantuml_code.append('@startuml')
        plantuml_code.append('!includeurl https://raw.githubusercontent.com/plantuml-stdlib/Archimate-PlantUML/master/Archimate.puml')
        system_group_elements = []
        program_group_elements = []        
        if program not in plantuml_code:
            plantuml_code.append(add_archimete_element(program, 'Component'))
        
        entrypoint = flow.get("EntryPoint")
        element = add_archimete_element(entrypoint, 'Function')
        if element not in plantuml_code:
            plantuml_code.append(element)
            relation = add_archimete_relation(program, entrypoint, 'Composition')
            if relation not in plantuml_code:
                plantuml_code.append(relation)

        for index, analyzed in enumerate(flow.get('AnalyzedPaths', [])):
            
            weight = analyzed.get('TotalWeight')
            call_to_systems, call_to_programs = process_external_systems(analyzed.get('CallsToExternalSystem', []))
            for item in call_to_programs:
                element = add_archimete_element(item, 'Component')
                if element not in program_group_elements:
                    program_group_elements.append(element)
                    program_group_elements.append(add_archimete_relation(entrypoint, item, 'Triggering'))

            for item in call_to_systems:               
                element = add_archimete_element(item, 'Component')
                if element not in system_group_elements:
                    system_group_elements.append(element)            
                    system_group_elements.append(add_archimete_relation(entrypoint, item, 'Triggering'))    
             
            
            for item in analyzed.get('CallsToEntryPoints', []):
                if item == entrypoint: continue
                element = add_archimete_element(item, 'Function')
                if element not in plantuml_code: 
                    plantuml_code.append(element)
                    relation = add_archimete_relation(entrypoint, item, 'Triggering')
                    if relation not in plantuml_code: 
                        plantuml_code.append(relation)

            external_calls = analyzed.get('CallsToExternalSystem', [])
            
            sl_diagram.append('@startuml')
            sl_diagram.append(f'|{SwimLanePallette.SKY_BLUE}|{program}|')
            if len(external_calls) > 0:
                sl_diagram.append(f'|{SwimLanePallette.SUNSET_RED}{EXTERNAL_CALL}')
            if any(item.startswith('XCTL') for item in external_calls):               
                sl_diagram.append(f'|{SwimLanePallette.GOLDEN_YELLOW}{OTHER_COBOL_PROGRAMS}') 
            sl_diagram.append(f'|{program}|')
            sl_diagram.append("start")
            for step in analyzed.get('Steps', []):
                if step in external_calls:
                    if step.startswith('XCTL'):
                        sl_diagram.append(OTHER_COBOL_PROGRAMS)
                    else:
                        sl_diagram.append(EXTERNAL_CALL)
                else:
                        sl_diagram.append(f'|{program}|')
                sl_diagram.append(f':{step};')
            sl_diagram.append("end")
            sl_diagram.append('@enduml')  
            sl_diagram_code = '\n '.join(sl_diagram)
            plant = PlantUML(url="http://www.plantuml.com/plantuml/svg/")
            print(sl_diagram_code)
            sl_svg = plant.processes(sl_diagram_code)
            
        
            output_dir = os.path.join(os.getcwd(), "output", "svg")
            os.makedirs(output_dir, exist_ok=True)
            sl_svg_path = os.path.join(output_dir, f"{program}_sl_diagram_{entrypoint}_{index}.svg")
            with open(sl_svg_path, 'wb') as f:
                f.write(sl_svg)
            sl_diagram.clear()
            
            
            
                        
            
        if len(system_group_elements) > 0:
            system_group = add_archimate_group('CICS', system_group_elements, 'CICS Systems')  
            plantuml_code.extend(system_group)
        if len(program_group_elements) > 0:
            program_group = add_archimate_group('Programs', program_group_elements, 'Other COBOL Programs') 
            plantuml_code.extend(program_group)
        plantuml_code.append('@enduml')
        diagram_code = '\n '.join(plantuml_code)
        plant = PlantUML(url="http://www.plantuml.com/plantuml/svg/")
        svg = plant.processes(diagram_code)
        
     
        output_dir = os.path.join(os.getcwd(), "output", "svg")
        os.makedirs(output_dir, exist_ok=True)
        svg_path = os.path.join(output_dir, f"{program}_diagram_{entrypoint}.svg")
        with open(svg_path, 'wb') as f:
            f.write(svg)


                      

            



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
def generate_diagram(program, entrypoint, internals, externals):
    plantuml_code=  []
    plantuml_code.append('@startuml')
    plantuml_code.append('!includeurl https://raw.githubusercontent.com/plantuml-stdlib/Archimate-PlantUML/master/Archimate.puml')
    plantuml_code.append(add_archimete_element(program, 'Component'))
    plantuml_code.append(add_archimete_element(entrypoint, 'Component'))
    
    for internal in internals:
        plantuml_code.append(add_archimete_element(internal, 'Function'))
        plantuml_code.append(add_archimete_relation(entrypoint, internal, 'Accesses'))
        
    # for external in externals:
    #     plantuml_code.append(f'[{entrypoint}] --> [{external}]')
    plantuml_code.append('@enduml')
    return plantuml_code
        
def sanitize_name(element_name):
    return element_name.replace(" ", "_").replace("-", "_").replace(".", "_").replace("(", "").replace(")", "").replace(",", "").replace("'", "").replace('"', "")
        
def add_archimete_element(element, type):
    return f'Application_{type}({sanitize_name(element)}, "{element}")'

def add_archimete_relation(from_element, to_element, type, direction=''):
    return f'Rel_{type}({sanitize_name(from_element)}, {sanitize_name(to_element)})'  

def add_archimate_group(group_name, elements, description=''):
    doc = []      
    group_name = sanitize_name(group_name)
    doc.append(f'Group({group_name}, "{description}") {{')
    doc.extend(elements)    
    doc.append('}')    
    return doc


def extract_calls(data):
    entry_points = {}
    external_systems = {}
    
    for flow in data['flow']:
        entry_point = flow['EntryPoint']
        entry_points[entry_point] = set()
        external_systems[entry_point] = set()
        
        for path in flow['AnalyzedPaths']:
            entry_points[entry_point].update(path['CallsToEntryPoints'])
            external_systems[entry_point].update(path['CallsToExternalSystem'])
    
    # Convert sets to sorted lists
    for ep in entry_points:
        entry_points[ep] = sorted(entry_points[ep])
        external_systems[ep] = sorted(external_systems[ep])
    
    return entry_points, external_systems




if __name__ == "__main__":
    json_path = os.path.join(os.path.dirname(__file__),  "..\..\..\output\Analyzed_DOGETRAN.json")
    data = process_json_data(json_path)
    generate_document(data)

    print("Analysis Completed!")     










