import os
from logger import logger
from parse_json import get_flow, is_condition, is_assignment, is_internal_call, is_external_call
import json





BASE_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), "../.."))
def process_json_data(json_file, entry_point):
    plantuml = []
    #Import Archimate puml
    plantuml.append('!includeurl https://raw.githubusercontent.com/plantuml-stdlib/Archimate-PlantUML/master/Archimate.puml')
    logger.debug(BASE_DIR)
    logger.debug("Current Working Directory:", os.getcwd())
    with open(json_file) as f:
        data = json.load(f)
    # plantuml.append("scale 1.5")
    program_name = data.get("ProgramId")
    plantuml.append(add_component(program_name))
    system = "" #data.get("Program").get("system", "")
    security = "" #data.get("Program").get("security", "")

    flow = get_flow(data, entry_point)
    if flow is None:
        logger.info(f'Flow {entry_point} not found in json!')
        return
    plantuml.append(add_interface(entry_point))
    plantuml.append(serving(program_name, entry_point, 'u'))
    
    flow_statements = flow.get("Statements")
    for statement in flow_statements:
        plantuml.extend(process_statement(data, statement, entry_point))

    with open("archi.plu", "w") as f:
        f.write("@startuml\n")
        for line in plantuml:
            logger.info(f"{line}")
            f.write(f"{line}\n")
        f.write("@enduml\n")        
        
        
def process_statement(data, statement, flow_name):
    plantuml = []
    if is_condition(statement):
        # plantuml.append(f'alt "{statement.get("methodName")}"')
        if statement.get('TrueStatements'):
            for st in statement.get('TrueStatements'):
                plantuml.extend(process_statement(data, st, flow_name ))
                # plantuml.append(f'"{flow_name}" -> "{st.get("methodName")}": "{st.get("methodName")}"')
            # if statement.get('FalseStatements'):                
            #     plantuml.append("else")
        if statement.get('FalseStatements'):
            for st in statement.get('FalseStatements'):
                plantuml.extend(process_statement(data, st, flow_name ))
                # plantuml.append(f'"{flow_name}" -> "{st.get("methodName")}": "{st.get("methodName")}"')
        # plantuml.append('end')
    # elif is_assignment(statement):
    #     plantuml.append(f'"{flow_name}" -> "{flow_name}": "{statement.get("methodName")}"')        
    elif is_internal_call(statement):
        plantuml.append(add_function(statement.get("methodName")))
    elif is_external_call(statement):   
        plantuml.append(f'Group({sanitize_name(statement.get("methodName"))}, "{statement.get("methodName")}"){{')
        plantuml.append(add_function(statement.get("methodName")))   
        external_flow = statement.get("Statements")
        if external_flow:
            for st in external_flow:
                plantuml.append(add_function(st.get("methodName")))
        plantuml.append('}')
                
    # else:
    #     plantuml.append(f'"{flow_name}" -> "{statement.get("methodName")}": "{statement.get("methodName")}"')        
    return plantuml

def sanitize_name(name):
    return name.replace('-','_').replace('(', '').replace(')', '').replace("'", '')
def add_component(name):
    return f'Application_Component({sanitize_name(name)}, "{name}")'

def add_function(name):
    return f'Application_Function({sanitize_name(name)}, "{name}")'

def add_interface(name):
    return f'Application_Interface({sanitize_name(name)}, "{name}")'

def realization(from_item, to_item, direction):
    return relationship('Realization', from_item, to_item, direction)

def flow(from_item, to_item, direction):
    return relationship('Flow', from_item, to_item, direction)

def serving(from_item, to_item, direction):
    return relationship('Serving', from_item, to_item, direction)

def access(from_item, to_item, direction):
    return relationship('Access', from_item, to_item, direction)

def aggregation(from_item, to_item, direction):
    return relationship('Aggregation', from_item, to_item, direction)

def assignment(from_item, to_item, direction):
    return relationship('Assignment', from_item, to_item, direction)

def association(from_item, to_item, direction):
    return relationship('Association', from_item, to_item, direction)

def composition(from_item, to_item, direction):
    return relationship('Composition', from_item, to_item, direction)

def influence(from_item, to_item, direction):
    return relationship('Influence', from_item, to_item, direction)

def realization(from_item, to_item, direction):
    return relationship('Realization', from_item, to_item, direction)

def triggering(from_item, to_item, direction):
    return relationship('Triggering', from_item, to_item, direction)





def relationship(relationship, from_item, to_item, direction):
    match direction:
        case 'u': return f'Rel_{relationship}_Up({sanitize_name(from_item)}, {sanitize_name(to_item)})'
        case 'd': return f'Rel_{relationship}_Down({sanitize_name(from_item)}, {sanitize_name(to_item)})'
        case 'l': return f'Rel_{relationship}_Left({sanitize_name(from_item)}, {sanitize_name(to_item)})'
        case 'r': return f'Rel_{relationship}_Right({sanitize_name(from_item)}, {sanitize_name(to_item)})'
        
        
        

# def save(diagram, file_path, output_dir="output"):
#     """ Αποθηκεύει τα αποτελέσματα της ανάλυσης σε JSON αρχείο """
#     logger.info(f"Saving file {file_path}...")
    
#     file_name = os.path.basename(file_path)
#     file_name_without_ext = os.path.splitext(file_name)[0]
#     # output_file = os.path.join(output_dir, f"{file_name_without_ext}.puml")
#     output_file = os.path.join(os.path.dirname(__file__), f"{file_name_without_ext}.puml")

#     with open(output_file, "w", encoding="utf-8") as f:
#         for line in diagram:
#             f.write(f"{line}\n")

#     print(f"Ανάλυση αποθηκεύτηκε στο: {output_file}")    

if __name__ == "__main__":
    # json_path = os.path.join(os.path.dirname(__file__),  "..\\cobol_parser\\output\\DOGEMAIN.json")
    # process_json_data(json_path, '00000-MAIN') 
    json_path = os.path.join(os.path.dirname(__file__),  "..\\cobol_parser\\output\\DOGESEND.json")
    process_json_data(json_path, 'DOGE-MAIN')
    
    print("Analysis Completed!")    