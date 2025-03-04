import os
from logger import logger
from parse_json import get_flow, is_condition, is_assignment, is_internal_call, is_external_call
import json




BASE_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), "../.."))
def process_json_data(json_file, entry_point):
    plantuml = []
    logger.debug(BASE_DIR)
    logger.debug("Current Working Directory:", os.getcwd())
    with open(json_file) as f:
        data = json.load(f)
    # plantuml.append("scale 1.5")
    program_name = data.get("ProgramId")
    plantuml.append(f"title {program_name}")
    plantuml.append("!theme materia")
    system = "" #data.get("Program").get("system", "")
    security = "" #data.get("Program").get("security", "")

    flow = get_flow(data, entry_point)
    if flow is None:
        logger.info(f'Flow {entry_point} not found in json!')
        return
    
    flow_statements = flow.get("Statements")
    for statement in flow_statements:
        plantuml.extend(process_statement(data, statement, entry_point))

    with open("sequence.plu", "w") as f:
        f.write("@startuml\n")
        for line in plantuml:
            logger.info(f"{line}")
            f.write(f"{line}\n")
        f.write("@enduml\n")        
        
        
def process_statement(data, statement, flow_name):
    plantuml = []
    if is_condition(statement):
        plantuml.append(f'alt "{statement.get("methodName")}"')
        if statement.get('TrueStatements'):
            for st in statement.get('TrueStatements'):
                plantuml.extend(process_statement(data, st, flow_name ))
                # plantuml.append(f'"{flow_name}" -> "{st.get("methodName")}": "{st.get("methodName")}"')
            if statement.get('FalseStatements'):                
                plantuml.append("else")
        if statement.get('FalseStatements'):
            for st in statement.get('FalseStatements'):
                plantuml.extend(process_statement(data, st, flow_name ))
                # plantuml.append(f'"{flow_name}" -> "{st.get("methodName")}": "{st.get("methodName")}"')
        plantuml.append('end')
    elif is_assignment(statement):
        plantuml.append(f'"{flow_name}" -> "{flow_name}": "{statement.get("methodName")}"')        
    elif is_internal_call(statement):
        plantuml.append(f'"{flow_name}" -> "{statement.get("methodName")}" : Execute "{statement.get("methodName")}"')  
        internal_flow = get_flow(data, statement.get("methodName"))
        if internal_flow:
            for st in internal_flow.get("Statements"):
                plantuml.extend(process_statement(data, st, statement.get("methodName") ))     
    elif is_external_call(statement):          
        plantuml.append(f'"{flow_name}" -> CICS : Execute "{statement.get("methodName")}" ')  
        external_flow = get_flow(data, statement.get("methodName"))
        if external_flow:
            for st in external_flow.get("Statements"):
                plantuml.extend(process_statement(data, st, "CICS" ))                              
    else:
        plantuml.append(f'"{flow_name}" -> "{statement.get("methodName")}": "{statement.get("methodName")}"')        
    return plantuml

if __name__ == "__main__":
    # json_path = os.path.join(os.path.dirname(__file__),  "..\\cobol_parser\\output\\DOGEMAIN.json")
    # process_json_data(json_path, '00000-MAIN') 
    json_path = os.path.join(os.path.dirname(__file__),  "..\\cobol_parser\\output\\DOGESEND.json")
    process_json_data(json_path, 'DOGE-MAIN')
    print("Analysis Completed!")    