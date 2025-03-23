import sys
import os
import json
import uuid
from logger import logger 
from models.statement import Statement
import networkx as nx
import matplotlib.pyplot as plt
from tree_node import TreeNode



EXTERNAL_CALL_RATE = 1.0
INTERNAL_CALL_RATE = 0.5
CONDITIONAL_RATE = 0.4
ASSINGMENT_RATE = 0.3
OTHER_RATE = 0.2

ST_ASSIGN = 1
ST_CALL = 2
ST_CONDITION = 3
ST_EXTERNAL = 4
ST_OTHER = 5

         
    


BASE_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), "../.."))
def process_json_data(json_file):
    with open(json_file) as f:
        data = json.load(f)
    return data

def add_statement(data, statement, kindof=None):
    node = TreeNode(statement, kindof)
    if statement['type'] == 'StatementType.CALL':
        node.methodName = node.methodName + " " + " ".join([nested_statement['methodName'] for nested_statement in statement['Statements']])

        # for nested_statement in statement['Statements']:
        #     nested_node = add_statement(node, nested_statement, kindof)
        #     node.add_child(nested_node)
    elif statement['type'] == 'StatementType.CONDITION':
        for nested_statement in statement['TrueStatements']:
            kindof = 'True Path'
            nested_node = add_statement(node, nested_statement, kindof)
            node.add_child(nested_node)
        for nested_statement in statement['FalseStatements']:
            kindof = 'False Path'
            nested_node = add_statement(node, nested_statement, kindof)
            node.add_child(nested_node)
    return node

def evaluate(json_data, entry_point):
    data = []
    flow = get_flow(json_data, entry_point)
    root = TreeNode(entry_point)
    for statement in flow['Statements']:
        current_node = add_statement(root, statement)

        # current_node = TreeNode(statement.get('methodName'))
        # if statement['type'] == 'StatementType.CALL':
        #     for nested_statement in statement['Statements']:
        #         nested_node = TreeNode(nested_statement.get('methodName'))
        #         current_node.add_child(nested_node)
        # elif statement['type'] == 'StatementType.CONDITION':
        #     for nested_statement in statement['TrueStatements']:
        #         nested_node = TreeNode(nested_statement.get('methodName'))
        #         current_node.add_child(nested_node)
        #     for nested_statement in statement['FalseStatements']:
        #         nested_node = TreeNode(nested_statement.get('methodName'))
        #         current_node.add_child(nested_node)
        root.add_child(current_node)
    
    tree_data = root.print_tree()
    for node in tree_data:
        logger.info(node)
    return
    data = evaluate_paragraph(json_data, flow['Statements'])
    sorted_data = sorted(data, key=lambda x: x.level, reverse=True)
    evaluated_data = apply_rating(sorted_data)
    generate_critical_paths(flow['Statements'][0].get('id'), evaluated_data)
    
    GG = nx.DiGraph()
    for sd in sorted_data:
        # logger.info(sd)
        if  sd.parentId and sd.previousStatementId:
            previous_name = ([item for item in sorted_data if item.id == sd.previousStatementId])[0]
            GG.add_edge(sd.previousStatementId, sd.id)
            # logger.info(f'G.add_edge({sd.previousStatementId}, {sd.id})')
            logger.info(f' {previous_name.level} : {previous_name.methodName} - {sd.level} :{sd.methodName}')
            # logger.info(f'G.add_edge({sd.previousStatementId}, {sd.id}) {previous_name.level} : {previous_name.methodName} - {sd.level} :{sd.methodName}')
        # elif sd.parentId:
        #     previous_name = [item.methodName for item in sorted_data if item.id == sd.previousStatementId]
        #     GG.add_edge(sd.previousStatementId, sd.id)
        #     logger.info(f'G.add_edge({sd.previousStatementId}, {sd.id}) {previous_name} - {sd.methodName}')
            
        
    return
    
    G = nx.DiGraph()
    for s in data:
        logger.info(s)
        if s.nextStatementId:
            G.add_edge(s.id, s.nextStatementId)
            logger.info(f'G.add_edge({s.id}, {s.nextStatementId})')
        else:
            for item in [x for x in data if x.previousStatementId == s.id]:
                G.add_edge(s.id, item.id)
                logger.info(f'G.add_edge({s.id}, {item.id})')

    
    # **Δημιουργία διάταξης κόμβων**
    pos = nx.spring_layout(G)  # Αυτό θα φτιάξει μια ωραία διάταξη

    # **Σχεδίαση του Graph**
    plt.figure(figsize=(12, 8))  # Ορίζει το μέγεθος της εικόνας

    options = {
        "with_labels": True,  # Δείχνει τα labels (ids των statements)
        "node_size": 3000,
        "node_color": "lightblue",
        "edge_color": "black",
        "linewidths": 2,
        "width": 2,
        "font_size": 10
    }

    nx.draw(G, pos, **options)  # Σχεδιάζει το γράφημα

    # Προσθήκη Labels
    labels = {s.id: s.methodName for s in data}
    nx.draw_networkx_labels(G, pos, labels, font_size=10)

    # Εμφάνιση του Γραφήματος
    plt.show()
    paths = list(nx.all_simple_paths(G, source=1, target=5))
    # for path in paths:
    #     print(" -> ".join(map(str, path)))    
    
    # entry_node = data[0].id  # Το πρώτο statement είναι το entry point
    # leaf_nodes = [s.id for s in data if s.nextStatementId is None]  # Βρίσκουμε τα τερματικά statements
            
    # for leaf in leaf_nodes:
    #     paths = list(nx.all_simple_paths(G, source=entry_node, target=leaf))
    #     for path in paths:
    #         logger.info(" -> ".join(map(str, path)))

    return data
    
def evaluate_paragraph(json_data, paragraph, parent_id=None, level = 0, next_id=None):
    if not paragraph:
        return []
    statements = []
    # previous_id =parent_id
    for idx, statement in enumerate(paragraph):
        previous_id = paragraph[idx - 1].get('id') if idx > 0 else parent_id
        next_id = paragraph[idx + 1].get('id') if idx + 1 < len(paragraph) else next_id
        match statement['type']:
            case 'StatementType.CONDITION': 
                statements.extend(process_condition(json_data, statement, parent_id, level, next_id))
            case 'StatementType.CALL': 
                statements.extend(process_call(json_data, statement, parent_id, level, next_id))
            case 'StatementType.ASSIGN': 
                statements.append(Statement(id=statement.get('id'), methodName=statement.get('methodName'),  parentId= parent_id, level=level, rate = ASSINGMENT_RATE,  type=ST_ASSIGN, previousStatementId=previous_id, nextStatementId=next_id))
            case 'StatementType.OTHER': 
                statements.append(Statement(id=statement.get('id'), methodName=statement.get('methodName'),  parentId= parent_id, level=level, rate = OTHER_RATE,  type=ST_OTHER, previousStatementId=previous_id, nextStatementId=next_id))
        previous_id = statement.get('id')
    return statements

    
def process_call(json_data, statement, parent_id, level, next_id):
    statements = []
    previous_id =parent_id    
    is_internal_call = statement.get('internal')  == True
    statements.append(Statement(id=statement.get('id'), methodName=statement.get('methodName'),  parentId= parent_id, level=level, rate = INTERNAL_CALL_RATE,  type=ST_CALL, previousStatementId=previous_id, nextStatementId=next_id))    
    nested_statements = statement.get('Statements')
    for idx, st in enumerate(nested_statements):
        id =  str(uuid.uuid4())
        next__internal_id = nested_statements[idx + 1].get('id') if idx + 1 < len(nested_statements) else next_id        
        statements.append(Statement(id=id, methodName=st.get('methodName'),  parentId=parent_id, level=level + 1, rate = EXTERNAL_CALL_RATE,  type=ST_CALL, previousStatementId=previous_id, nextStatementId=next__internal_id))
        previous_id = st.get('id')
    return statements                


def process_condition(json_data, statement, parent_id, level, next_id):
    statements = []
    previous_id =parent_id      
    statements.append(Statement(id=statement.get('id'), methodName=statement.get('methodName'),  parentId= parent_id, level=level, rate = ASSINGMENT_RATE,  type=ST_CONDITION, previousStatementId=previous_id))
    
    if statement['TrueStatements']:
        statements.extend(evaluate_paragraph(json_data, statement['TrueStatements'], statement.get('id'), level + 1, next_id))
    if statement['FalseStatements']: 
        statements.extend(evaluate_paragraph(json_data, statement['FalseStatements'], statement.get('id'), level + 1, next_id))
    return statements
    
def apply_rating(sorted_data):
    for data in sorted_data:
        if data.level > 1:
            parent_statement = next((item for item in sorted_data if item.id == data.parentId), None)
            if parent_statement:
                data.rate += parent_statement.rate
    return sorted_data
    # for data in sorted_data:
    #     logger.info(f"{data.methodName}:{data.rate}")     
        
def generate_critical_paths(entry_point_id, evaluated_data):           
    use_cases = []
    critical_path = []
    findid = next((item for item in evaluated_data if item.id == entry_point_id), 0)
    
    logger.info(critical_path)
        
    
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
    data = evaluate(data, '00000-MAIN')
    print("Analysis Completed!")     
    
    


        
    