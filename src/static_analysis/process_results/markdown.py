import os
import json
from logger import logger 

BASE_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), "../.."))

def process_json_data(json_file):
    with open(json_file) as f:
        data = json.load(f)
    return data

def generate_document(data):
    output_dir = os.path.join(os.getcwd(), "output")
    os.makedirs(output_dir, exist_ok=True)
    doc = []
    program = data.get("program")
    doc.append(add_header(program))
    doc.append(add_separator())
    doc.append(f'{add_bold("File")}: {data.get("document")}.cbl')
    doc.append(f'{add_bold("Program")}: {program}')
    doc.append(f'{add_bold("Language")}: COBOL')
    doc.append(add_separator())
    
    entry_points, external_systems = extract_calls(data)
    
    # Generate diagrams for each entry point
    for flow in data.get('flow', []):
        entrypoint = flow.get("EntryPoint")
        doc.append(add_header(entrypoint, level=2))
        doc.append(add_separator())
        
        # Add entry point description
        doc.append(f'{add_bold("Description")}: Main entry point for {entrypoint} functionality')
        
        # Add internal and external calls
        doc.append(f'\n{add_bold("Internal Calls")}:')
        doc.extend([f'- {ep}' for ep in entry_points.get(entrypoint, [])])
        
        doc.append(f'\n{add_bold("External Calls")}:')
        doc.extend([f'- {ext}' for ext in external_systems.get(entrypoint, [])])
        
        # Generate and add PlantUML diagram

        doc.append('\n' + add_bold("Archimate Diagram:"))
        doc.append(add_diagram_svg(f"svg/{program}_diagram_{entrypoint}.svg"))
        
        # Add analyzed paths
        doc.append(add_header("Analyzed Paths", level=3))
        for index, analyzed in enumerate(flow.get('AnalyzedPaths', [])):
            doc.append(add_separator())
            doc.append(f'{add_bold("Use case")} (Weight: {analyzed.get("TotalWeight")})')
            
            # Add business rules
            if analyzed.get("BusinessRules"):
                doc.append('\n' + add_bold("Business Rules:"))
                doc.extend(add_code(analyzed["BusinessRules"]))
            
            # Add steps
            doc.append('\n' + add_bold("Execution Path:"))
            doc.append(add_diagram_svg(f"svg/{program}_sl_diagram_{entrypoint}_{index}.svg"))
        
        doc.append('\n')
    
    # Save the markdown document
    output_path = os.path.join(output_dir, f"{program}_analysis_report.md")
    with open(output_path, 'w') as f:
        f.write('\n'.join(doc))
    
    logger.info(f"Analysis report generated at: {output_path}")
    return doc

def process_external_systems(external_systems):
    cics_commands = []
    programs = []
    
    for cmd in external_systems:
        if cmd.startswith('XCTL PROGRAM'):
            # Extract program name from XCTL commands
            program = cmd.split("'")[1]
            programs.append(program)
        elif any(cmd.startswith(prefix) for prefix in ['RETURN', 'SEND', 'RECEIVE', 'STARTBR', 'READ', 'ENDBR', 'RESETBR']):
            cics_commands.append(cmd.split(' ')[0])
        elif cmd.startswith('WRITE OPERATOR'):
            cics_commands.append('WTO')
        elif cmd.startswith('FORMATTIME'):
            cics_commands.append('FORMATTIME')
    
    return sorted(list(set(cics_commands))), sorted(list(set(programs)))

def generate_diagram(program, entrypoint, internals, externals):
    output_dir = os.path.join(os.getcwd(), "output")
    os.makedirs(output_dir, exist_ok=True)
    plantuml_code = []
    plantuml_code.append('@startuml')
    plantuml_code.append('!includeurl https://raw.githubusercontent.com/plantuml-stdlib/Archimate-PlantUML/master/Archimate.puml')
    
    # Main program component
    plantuml_code.append(f'Application_Component({sanitize_name(program)}, "{program}")')
    
    # Entry point function
    plantuml_code.append(f'Application_Function({sanitize_name(entrypoint)}, "{entrypoint}")')
    plantuml_code.append(f'Rel_Composition({sanitize_name(program)}, {sanitize_name(entrypoint)})')
    
    # Internal functions
    for internal in internals:
        if internal != entrypoint:  # Skip the entrypoint itself
            plantuml_code.append(f'Application_Function({sanitize_name(internal)}, "{internal}")')
            plantuml_code.append(f'Rel_Triggering({sanitize_name(entrypoint)}, {sanitize_name(internal)})')
    
    # Process external systems
    cics_commands, programs = process_external_systems(externals)
    
    # Add CICS services group
    if cics_commands:
        plantuml_code.append('\nGroup(CICS, "CICS Services") {')
        for cmd in cics_commands:
            plantuml_code.append(f'  Application_Component({sanitize_name(cmd)}, "{cmd}")')
            plantuml_code.append(f'  Rel_Serving({sanitize_name(cmd)}, {sanitize_name(entrypoint)})')
        plantuml_code.append('}')
    
    # Add other COBOL programs group
    if programs:
        plantuml_code.append('\nGroup(COBOL_Programs, "Other COBOL Programs") {')
        for prog in programs:
            plantuml_code.append(f'  Application_Component({sanitize_name(prog)}, "{prog}")')
        plantuml_code.append('}')
        
        # Connect entry point to programs
        for prog in programs:
            plantuml_code.append(f'Rel_Triggering({sanitize_name(entrypoint)}, {sanitize_name(prog)})')
    
    plantuml_code.append('@enduml')
    
    # Save the PlantUML diagram

    diagram_path = os.path.join(output_dir, f"diagram_{sanitize_name(entrypoint)}.puml")
    with open(diagram_path, 'w') as f:
        f.write('\n'.join(plantuml_code))
    
    logger.info(f"PlantUML diagram generated at: {diagram_path}")
    return plantuml_code

def sanitize_name(element_name):
    return element_name.replace(" ", "_").replace("-", "_").replace(".", "_") \
                      .replace("(", "").replace(")", "").replace(",", "") \
                      .replace("'", "").replace('"', "").replace("=", "")

def add_separator():
    return '---'

def add_header(label, level=1):
    return f'{"#" * level} {label}'

def add_bold(label):
    return f'**{label}**'

def add_code(lines, language=''):
    code = [f'```{language}'] + lines + ['```']
    return code
def add_diagram_svg(diagram_path):
    return f'![Diagram]({diagram_path})'

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
    json_path = os.path.join(os.path.dirname(__file__),  "..\..\..\output\\Analyzed_DOGETRAN.json")
    # json_path = os.path.join(BASE_DIR, "output", "Analyzed_DOGEMAIN.json")
    data = process_json_data(json_path)
    generate_document(data)
    print("Analysis Completed!")




























# ###++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# import os
# import json
# from logger import logger 


# BASE_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), "../.."))
# def process_json_data(json_file):
#     with open(json_file) as f:
#         data = json.load(f)
#     return data
# def generate_document(data):
#     doc = []
#     program = data.get("program")
#     doc.append(add_header(program))
#     doc.append(add_separator())
#     doc.append(f'{add_bold("File")}: {data.get("program")}')
#     doc.append(f'{add_bold("Language")}: COBOL')
#     doc.append(add_separator())
#     flows = data.get('flow', [])
#     entry_points, external_systems = extract_calls(data)
#     for flow in flows:
#         entrypoint = flow.get("EntryPoint")
#         doc.append(f'{add_bold("Entry-Point")}: {entrypoint}')
#         section_entry_points = entry_points[entrypoint]    
#         section_external_systems = external_systems[entrypoint]    
#         a = generate_diagram(program, entrypoint, section_entry_points, section_external_systems)    
#         for analyzed in flow.get('AnalyzedPaths', []):
#             weight = analyzed.get('TotalWeight')
#             doc.append(f'{add_bold("Use case")} \t Weight: {weight}')
#             br = analyzed.get('BusinessRules',[])
#             doc.extend(add_code(br))
#             call_to_systems, call_to_programs = process_external_systems(section_external_systems)
#             system_group_elements = []
#             program_group_elements = []
#             for system in call_to_systems:
#                 system_group_elements.append(add_archimete_element(system, 'Component'))
#             for p in call_to_programs:
#                 program_group_elements.append(add_archimete_element(p, 'Component'))       
#             system_group = add_archimate_group('CICS', system_group_elements, 'CICS Systems')  
#             program_group = add_archimate_group('Programs', program_group_elements, 'Other COBOL Programs')    
               
#             doc.extend('')


# def process_external_systems(external_systems):
#     cics = []
#     programs = []
    
#     for cmd in external_systems:
#         splitted = cmd.split(' ')
#         if len(splitted) > 1:
#             if splitted[0] == 'XCTL':
#                 programs.append(splitted[1])
#             else:
#                 cics.append('{splitted[0]}')
#     return cics, programs
# def generate_diagram(program, entrypoint, internals, externals):
#     plantuml_code=  []
#     plantuml_code.append('@startuml')
#     plantuml_code.append('!includeurl https://raw.githubusercontent.com/plantuml-stdlib/Archimate-PlantUML/master/Archimate.puml')
#     plantuml_code.append(add_archimete_element(program, 'Component'))
#     plantuml_code.append(add_archimete_element(entrypoint, 'Component'))
    
#     for internal in internals:
#         plantuml_code.append(add_archimete_element(internal, 'Function'))
#         plantuml_code.append(add_archimete_relation(entrypoint, internal, 'Accesses'))
        
#     # for external in externals:
#     #     plantuml_code.append(f'[{entrypoint}] --> [{external}]')
#     plantuml_code.append('@enduml')
#     return plantuml_code
        
# def sanitize_name(element_name):
#     return element_name.replace(" ", "_").replace("-", "_").replace(".", "_").replace("(", "").replace(")", "").replace(",", "").replace("'", "").replace('"', "")
        
# def add_archimete_element(element, type):
#     return f'Application_{type}({sanitize_name(element)}, "{element}")'

# def add_archimete_relation(from_element, to_element, type, direction=''):
#     return f'Rel_{type}({sanitize_name(from_element)}, {sanitize_name(to_element)})'  

# def add_archimate_group(group_name, elements, description=''):
#     doc = []      
#     group_name = sanitize_name(group_name)
#     doc.append(f'Group({group_name}, "{description}") {{')
#     doc.extend(elements)    
#     doc.append('}')    
#     return doc


# def add_separator():
#     return('---')
# def add_header(label, level=1):
#     tag = '#'*level
#     return f'{tag} {label}'
# def add_bold(label):
#     return f'**{label}**'
# def add_code(lines, language=''):
#     result = []
#     result.append(f'```{language}')
#     for line in lines:
#         result.append(line)
#     result.append('```')
#     return result

# def extract_calls(data):
#     entry_points = {}
#     external_systems = {}
    
#     for flow in data['flow']:
#         entry_point = flow['EntryPoint']
#         entry_points[entry_point] = set()
#         external_systems[entry_point] = set()
        
#         for path in flow['AnalyzedPaths']:
#             entry_points[entry_point].update(path['CallsToEntryPoints'])
#             external_systems[entry_point].update(path['CallsToExternalSystem'])
    
#     # Convert sets to sorted lists
#     for ep in entry_points:
#         entry_points[ep] = sorted(entry_points[ep])
#         external_systems[ep] = sorted(external_systems[ep])
    
#     return entry_points, external_systems




# if __name__ == "__main__":
#     json_path = os.path.join(os.path.dirname(__file__),  "..\..\..\output\\Analyzed_DOGEMAIN.json")
#     data = process_json_data(json_path)
#     generate_document(data)

#     print("Analysis Completed!")     