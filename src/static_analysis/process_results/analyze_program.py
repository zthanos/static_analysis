import sys
import os
import json


BASE_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), "../.."))
def process_json_data(json_file):
    with open(json_file) as f:
        data = json.load(f)
    return data

def evaluate(json_data, entry_point):
    struct = []
    
    flow = get_flow(json_data, entry_point)
    for statement in flow['Statements']:
        rate=0
        match statement['type']:
            case 'StatementType.CONDITION': rate = 30
            case 'StatementType.CALL': rate = 50
            case 'StatementType.ASSIGN': rate = 10
            case 'StatementType.OTHER': rate = 5
        a = {'description': statement['methodName'], 'type': statement['type'], 'rate': rate}
        print(a)
    
    # print(flow)
    
def get_flow(data, flow_name):
    main_node = next((node for node in data.get("Flows", []) if node.get("Name") == flow_name), None)
    if main_node:
        return main_node
        # print(json.dumps(main_node, indent=4))
    else:
        print("Node '00000-MAIN' not found.")
    return None
    

if __name__ == "__main__":
    json_path = os.path.join(os.path.dirname(__file__),  "..\\process_results\\DOGEMAIN.json")
    data = process_json_data(json_path)
    evaluate(data, '00000-MAIN')
    print("Analysis Completed!")     