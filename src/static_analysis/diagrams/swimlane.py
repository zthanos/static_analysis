import sys
import os
import json
from logger import logger 

BASE_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), "../.."))
def process_json_data(json_file, entry_point):
    plantuml = []
    logger.debug(BASE_DIR)
    logger.debug("Current Working Directory:", os.getcwd())
    with open(json_file) as f:
        data = json.load(f)
    plantuml.append("scale 1.5")
    program_name = data.get("ProgramId")
    plantuml.append(f"title {program_name}")
    plantuml.append("!theme materia")
    system = "" #data["Program"].get("system", "")
    security = "" #data["Program"].get("security", "")

    flow = get_flow(data, entry_point)
    if flow is None:
        logger.info(f'Flow {entry_point} not found in json!')
        return
    
    plantuml.append(f'|{flow.get("Name")}|')
    flow_statements = flow.get("Statements")
    plantuml.append("start")
    for statement in flow_statements:
        plantuml.extend(process_statement(data, statement, flow.get("Name")))
    plantuml.append("end")

    with open("output.plu", "w") as f:
        f.write("@startuml\n")
        for line in plantuml:
            logger.info(f"{line}")
            f.write(f"{line}\n")
        f.write("@enduml\n")            


def process_statement(data, statement, lane, expanded = True):
    plantuml=[]
    plantuml.append(f'|{lane}|')
    if is_condition(statement):
        plantuml.append(f'  if ({statement.get("methodName")}) then (true)')
        for st in statement.get('TrueStatements'):
            plantuml.extend(process_statement(data, st, lane))
        if statement.get("FalseStatements"):
            plantuml.append("else (false)")
            for st in statement.get("FalseStatements"):
                plantuml.extend(process_statement(data, st, lane))
    elif is_internal_call(statement):
        if expanded:
            internal_flow = get_flow(data, statement.get('methodName'))
            if internal_flow:
                for statement in internal_flow.get('Statements'):
                    plantuml.extend(process_statement(data, statement, internal_flow.get('Name')))        
        else:
            plantuml.append(f':{statement.get("methodName")};')
    elif is_external_call(statement):
        # if expanded:
        external_flow = statement.get("Statements")
        plantuml.append(f'|CICS|')
        plantuml.append(f":{statement.get("methodName")};")
        
        for statement in external_flow:
            if statement.get("Statements"):
                # plantuml.append(f'|{external_flow.get("methodName")}|')
                plantuml.append(f":{statement.get("methodName")};")
                # plantuml.extend(process_statement(data, statement, external_flow.get("methodName")))        
    else:
        plantuml.append(f":{statement.get("methodName")};")            


    return plantuml;
        
def get_flow(data, flow_name):
    main_node = next((node for node in data.get("Flow", []) if node.get("Name") == flow_name), None)
    if main_node:
        return main_node
        # print(json.dumps(main_node, indent=4))
    else:
        logger.info(f"Node '{flow_name}' not found.")
    return None


def is_condition(statement):
    return statement.get("type") == "StatementType.CONDITION"

def is_internal_call(statement):
    return statement.get('type') == "StatementType.CALL" and statement.get("internal") == True

def is_external_call(statement):
    return statement.get('type') == "StatementType.CALL" and statement.get("internal") == False
                
if __name__ == "__main__":
    json_path = os.path.join(os.path.dirname(__file__),  "..\\cobol_parser\\output\\DOGEMAIN.json")
    # print(json_path)
    # with open(json_path) as f:
    #     data = json.load(f)

    process_json_data(json_path, '00000-MAIN')      
    print("Analysis Completed!")          