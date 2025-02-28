import sys
import os
import json

BASE_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), "../.."))
def process_json_data(json_file, entry_point):
    plantuml = []
    print(BASE_DIR)
    print("Current Working Directory:", os.getcwd())
    with open(json_file) as f:
        data = json.load(f)
    
    program_name = data["Program"]
    plantuml.append(f"title {program_name}")
    plantuml.append("!theme materia")
    system = "" #data["Program"].get("system", "")
    security = "" #data["Program"].get("security", "")
    
    flow = get_flow(data, entry_point)
    plantuml.append(f"|{flow["Name"]}|")
    flow_statements = flow["Statements"]
    plantuml.append("start")
    for statement in flow_statements:
        plantuml.extend(process_statement(data, statement, flow["Name"]))
    plantuml.append("end")

    with open("output.plu", "w") as f:
        f.write("@startuml\n")
        for line in plantuml:
            print(f"{line}")
            f.write(f"{line}\n")
        f.write("@enduml\n")            

def process_statement(data, statement, lane, expanded = True):
    plantuml=[]
    if is_condition(statement):
        plantuml.append(f"  if ({statement["methodName"]}) then (true)")
        for st in statement["TrueStatements"]:
                plantuml.append(f"|{lane}|")            
                plantuml.extend(process_statement(data, st, lane))
        if statement["FalseStatements"]:
            plantuml.append("else (false)")
            for st in statement["FalseStatements"]:
                plantuml.append(f"|{lane}|")            
                plantuml.extend(process_statement(data, st, lane))
    elif is_internal_call(statement):
        if expanded:
            internal_flow = get_flow(data, statement["methodName"])
            plantuml.append(f"|{statement["methodName"]}|")
            for statement in internal_flow["Statements"]:
                plantuml.extend(process_statement(data, statement, lane))        
        else:
            plantuml.append(f":{statement["methodName"]};")
    elif is_external_call(statement):
        # if expanded:
        external_flow = statement["Statements"]
        plantuml.append(f"|{statement["methodName"]}|")
        for statement in external_flow:
        
            plantuml.extend(process_statement(data, statement, lane))        
        # else:
        #     plantuml.append(f":{statement["methodName"]};")            

    else:
        plantuml.append(f":{statement["methodName"]};")
    return plantuml;
        
def get_flow(data, flow_name):
    main_node = next((node for node in data.get("Flows", []) if node.get("Name") == flow_name), None)
    if main_node:
        return main_node
        # print(json.dumps(main_node, indent=4))
    else:
        print("Node '00000-MAIN' not found.")
    return None


def is_condition(statement):
    return statement["type"] == "StatementType.CONDITION"

def is_internal_call(statement):
    return statement["type"] == "StatementType.CALL"    

def is_external_call(statement):
    return statement["type"] == "StatementType.CICS"    
                
if __name__ == "__main__":
    json_path = os.path.join(os.path.dirname(__file__),  "..\\process_results\\DOGEMAIN.json")
    # print(json_path)
    # with open(json_path) as f:
    #     data = json.load(f)

    process_json_data(json_path, '00000-MAIN')      
    print("Analysis Completed!")          