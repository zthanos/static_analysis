import argparse
import glob
import os
import json
from logger import logger
from typing import List, Dict, Any
from tree_node import TreeNode
from tree import MyTree
from constants import *
import time


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


def evaluate(document, json_data, entry_point):
    start_time = time.time()
    flow = get_flow(json_data, entry_point)
    root = TreeNode(entry_point, kindof='Entry Point')
    tree = MyTree()
    tree.add_root(root)

    previous_node = root
    for statement in flow['Statements']:
        child_node = add_statement(statement)
        previous_node.add_child(child_node)
    tree.print_tree()
    possible_paths = tree.get_unique_paths_with_conditions()
    # for possible_path in possible_paths:
    #     logger.debug(possible_path['condition'])
    #     logger.debug(possible_path['path'])
    
    end_time = time.time()
    total_seconds = end_time - start_time
    logger.info(f"Start Time: {start_time}")
    logger.info(f"End Time: {end_time}")
    logger.info(f"Total Seconds: {total_seconds}")

    return [path['path'] for path in possible_paths]

    # tree.print_paths()
def analyze_document(document, json_data):
    entry_points_map=[]
    raw = []
    program = json_data.get('ProgramId')
    for flow in json_data.get("Flow", []):
        flow_name = flow.get("Name")    
        entry_points_map.append(flow_name)
    
    for flow in json_data.get("Flow", []):
        flow_name = flow.get("Name")
        
        if flow_name:
            logger.info(f"Evaluating flow: {flow_name}")
            paths = evaluate(document, json_data, flow_name)
            raw.append(analyze_paths(flow_name, paths, entry_points_map))
        
    return {"document": document, "program": program, "flow": raw }
    

def get_flow(data, flow_name):
    main_node = next((node for node in data.get("Flow", []) if node.get("Name") == flow_name), None)
    if main_node:
        return main_node
        # print(json.dumps(main_node, indent=4))
    else:
        logger.info(f"Node '{flow_name}' not found.")
    return None




# Sample path node for type hinting
Step = Dict[str, Any]
Path = List[Step]


def analyze_paths(entry_point_name: str, paths: List[Path], entry_points_map) -> Dict[str, Any]:
    """
    For a given entry point, analyze its paths and extract:
    - Total weight
    - Business rules (conditions)
    - Steps (excluding conditions and True/False paths)
    - Internal entry point calls
    """
    analyzed_paths = []

    for path in paths:
        total_weight = 0.0
        business_rules = []
        steps = []
        internal_calls = set()
        external_calls = set()

        for step in path:
            description = step["Description"]
            step_type = step["Type"]
            weight = step.get("Weight", 0.0)
            condition_value = step.get("ConditionValue")

            total_weight += weight

            if step_type == "StatementType.CONDITION":
                business_rules.append(f"{description} = {condition_value}")
            elif "True Path" in description or "False Path" in description:
                continue
            else:
                steps.append(description)
                # Detect if this step calls another entry point
                if description in entry_points_map:
                    internal_calls.add(description)
                else:
                    if step.get('External', False):
                        external_system = step.get('External', '')
                        external_calls.add(description)

        analyzed_paths.append({
            "TotalWeight": round(total_weight, 2),
            "BusinessRules": business_rules,
            "Steps": steps,
            "CallsToEntryPoints": sorted(list(internal_calls)),
            "CallsToExternalSystem": sorted(list(external_calls))
        })

    return {
        "EntryPoint": entry_point_name,
        "AnalyzedPaths": analyzed_paths
    }


def process_files(file_pattern):
    script_dir = os.path.dirname(os.path.abspath(__file__))
    pattern = os.path.join(script_dir, file_pattern)
    
    files = glob.glob(pattern)
    if not files:
        print(f"Δεν βρέθηκαν αρχεία που να ταιριάζουν με το μοτίβο: {file_pattern}")
        return

    for file_path in files:
        process_json_file(file_path)
    
def process_json_file(file_path):
    with open(file_path) as f:
        json_data = json.load(f)
    file_name = os.path.basename(file_path)
    file_name_without_ext = os.path.splitext(file_name)[0]
    output_dir = os.path.join(os.getcwd(), "output")
    os.makedirs(output_dir, exist_ok=True)
    output_file = os.path.join(output_dir, f"Analyzed_{file_name_without_ext}.json")
    
    logger.info(f"Επεξεργασία αρχείου: {file_path}")
    
    json_output = analyze_document(file_name_without_ext, json_data)

    with open(output_file, "w", encoding="utf-8") as f:
        json.dump(json_output, f, ensure_ascii=False, indent=4)

    print(f"Ανάλυση αποθηκεύτηκε στο: {output_file}")    



    
if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Static Analysis. JSON analyzer")
    # Ορισμός argument για το όνομα αρχείου ή wildcard pattern
    parser.add_argument("file_pattern", help="Όνομα αρχείου json ή wildcard pattern (π.χ. '*.json')")
    try:
        args = parser.parse_args()
        process_files(args.file_pattern)
    except Exception as e:
        logger.error(f"Σφάλμα κατά την εκτέλεση: {e}")    
    print("Analysis Completed!")





