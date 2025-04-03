import os
import json
from logger import logger

from tree_node import TreeNode
from tree import MyTree

from constants import *


BASE_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), "../.."))
def process_json_data(json_file):
    with open(json_file) as f:
        data = json.load(f)
    return data

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
        logger.debug(possible_path['condition'])
        logger.debug(possible_path['path'])
    return [path['path'] for path in possible_paths]

    # tree.print_paths()
def analyze_document(json_data):
    raw = []
    for flow in json_data.get("Flow", []):
        flow_name = flow.get("Name")
        if flow_name:
            logger.info(f"Evaluating flow: {flow_name}")
            paths = evaluate(json_data, flow_name)
            raw.append({'EntryPoint': flow_name, 'Paths': paths})
    return raw
    

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
    # data = evaluate(data, 'DOGE-WTO')
    res = analyze_document(data)
    for r in res:
        logger.info(r)
    print("Analysis Completed!")





