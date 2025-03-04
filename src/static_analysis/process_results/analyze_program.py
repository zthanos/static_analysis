import sys
import os
import json
from logger import logger 

BASE_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), "../.."))
def process_json_data(json_file):
    with open(json_file) as f:
        data = json.load(f)
    return data

def evaluate(json_data, entry_point):
    struct = []
    flow = get_flow(json_data, entry_point)
    rate = evaluate_paragraph(json_data, flow['Statements'])
    return rate
    
def evaluate_paragraph(json_data, paragraph):
    if not paragraph:
        return 0
    for statement in paragraph:
        rate=0
        true_path=0
        false_path=0
        match statement['type']:
            case 'StatementType.CONDITION': 
                true_path, false_path = process_condition(json_data, statement)
                rate = true_path +false_path
            case 'StatementType.CALL': 
                rate = process_call(json_data, statement)
            case 'StatementType.ASSIGN': 
                rate = 10
            case 'StatementType.OTHER': 
                rate = 5
        a = {'description': statement['methodName'], 'type': statement['type'], 'rate': rate, 'true_path': true_path, 'false_path': false_path}
        logger.info(a)
    return rate

    
def process_call(json_data, statement):
    is_internal_call = statement['internal']  == True
    rate = 40 if is_internal_call  == True else 50
    return rate
def process_condition(json_data, statement):
    true_rate = 0
    false_rate = 0
    if statement['TrueStatements']:
        true_rate = evaluate_paragraph(json_data, statement['TrueStatements'])
    if statement['FalseStatements']: 
        false_rate = evaluate_paragraph(json_data, statement['FalseStatements'])
    print(f"{statement['methodName']} true_score: {true_rate} false_score: {false_rate}")
    return true_rate, false_rate
    
    
def get_flow(data, flow_name):
    main_node = next((node for node in data.get("Flow", []) if node.get("Name") == flow_name), None)
    if main_node:
        return main_node
        # print(json.dumps(main_node, indent=4))
    else:
        logger.info(f"Node '{flow_name}' not found.")
    return None
    

if __name__ == "__main__":
    json_path = os.path.join(os.path.dirname(__file__),  "..\\cobol_parser\\output\\DOGEMAIN.json")
    data = process_json_data(json_path)
    evaluate(data, '00000-MAIN')
    print("Analysis Completed!")     