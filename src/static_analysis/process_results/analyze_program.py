import os
import json
from logger import logger 

import re
from tree_node import TreeNode
from tree import MyTree

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

def add_statement_old(data, statement, kindof=None):
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

def add_statement(statement, kindof=None):
    new_node = TreeNode(statement, kindof)

    if statement['type'] == 'StatementType.CALL':
        nested_method_names = " ".join(
            nested_statement['methodName']
            for nested_statement in statement.get('Statements', [])
        )
        if nested_method_names:
            new_node.methodName += " " + nested_method_names

    if statement['type'] == 'StatementType.CONDITION':
        # True Path Node
        true_path_node = TreeNode("True Path", kindof="False Path")
        previous_node = true_path_node
        for nested_statement in statement.get('TrueStatements', []):
            child_node = add_statement(nested_statement)
            previous_node.add_child(child_node)

        # False Path Node
        false_path_node = TreeNode("False Path", kindof="False Path")
        previous_node = false_path_node
        for nested_statement in statement.get('FalseStatements', []):
            child_node = add_statement(nested_statement)
            previous_node.add_child(child_node)

        # Προσθέτω τα True/False paths στο condition
        if true_path_node.children:
            new_node.add_child(true_path_node)
        if false_path_node.children:
            new_node.add_child(false_path_node)

    return new_node


def evaluate(json_data, entry_point):
    flow = get_flow(json_data, entry_point)
    root = TreeNode(entry_point, kindof='Entry Point')
    tree = MyTree()
    tree.add_root(root)
    
    previous_node = root
    for statement in flow['Statements']:
        child_node = add_statement(statement)
        previous_node.add_child(child_node)
    tree.print_tree()
    possible_paths = tree.get_unique_paths_with_conditions();
    for possible_path in possible_paths:
        logger.info(possible_path['condition'])
        logger.info(possible_path['path'])
        
    # tree.print_paths()
    
    
    
    
# Παίρνεις όλα τα paths
    # all_paths = root.get_all_paths()

    # # Τα εμφανίζεις καθαρά και ωραία:
    # for idx, path in enumerate(all_paths, 1):
    #     logger.info(f"Use Case {idx}:")
    #     logger.info("  ".join(path))
    #     logger.info("-" * 50)    
    # return
    # for statement in flow['Statements']:
    #     current_node = add_statement(tree.root, statement)
    #     tree.add_child(current_node)
    # tree.print_tree()
    # tree.print_paths()
        
        # root.add_child(current_node)
    # a = root.paths()
    # logger.info(a)
    # all_paths = root.dfs_paths()
    # for extracted_path in all_paths:
    #     logger.info(extracted_path)
    # paths_as_strings = [''.join(path) for path in all_paths]
    
    # logger.info(paths_as_strings)    
    
    # tree_data = root.print_tree()
    # for node in tree_data:
    #     logger.info(node)
    


    
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
    
    


        
    
def is_condition(statement):
    return statement.get("type") == "StatementType.CONDITION"

def is_assignment(statement):
    return statement.get("type") == "StatementType.ASSIGN"

def is_internal_call(statement):
    return statement.get('type') == "StatementType.CALL" and statement.get("internal") == True

def is_external_call(statement):
    return statement.get('type') == "StatementType.CALL" and statement.get("internal") == False    