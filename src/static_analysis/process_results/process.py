import logging
import sys
import os

# Βρες το path του "src" και πρόσθεσέ το στο sys.path
BASE_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), "../.."))
sys.path.append(BASE_DIR)

import json
from static_analysis.process_results import db_connection, db_context
from static_analysis import logger
from static_analysis.process_results import db_connection, db_context
from static_analysis import logger
import os
from enum import Enum


class StatementPosition(Enum):
    FIRST = "first"
    MIDDLE = "middle"
    LAST = "last"
logger = logger.setup_logger('StaticAnalysisLogger', 'static_analysis.log')


  
    
def process_json_data(json_file):
    """
    Parses a JSON file containing program flow information and inserts it into the Neo4j database.
    
    Parameters:
        json_file (str): Path to the JSON file containing static analysis data.
    """    
    print(BASE_DIR)
    print("Current Working Directory:", os.getcwd())
    with open(json_file) as f:
        data = json.load(f)
    driver = db_connection.get_neo4j_driver()
    program_name = data["Program"]
    system = "" #data["Program"].get("system", "")
    security = "" #data["Program"].get("security", "")
    relationships = []
    with driver.session() as session:
        session.execute_write(db_context.create_program, program_name, system, security)
        for flow in data["Flows"]:
            flow_name = flow["Name"] if "Name" in flow else ""
            input = flow.get("Input", {})
            output = flow.get("Output", {})
            flow_statements = flow["Statements"]
            session.execute_write(db_context.create_flow, program_name, flow_name, input, output)

            for flow_idx, statement in enumerate(flow_statements):
                session.execute_write(db_context.create_statement, statement)
            # Flow executes first statement
            session.execute_write(db_context.create_flow_to_statement_relationship, flow_name, flow_statements[0]['id'])

            for flow_idx, statement in enumerate(flow_statements):
                if (flow_idx < len(flow_statements) - 1) and not is_condition(statement):
                    session.execute_write(db_context.create_relationship, statement['id'], flow_statements[flow_idx + 1]['id'], 'EXECUTES')                    
                if is_condition(statement):
                    next_statement_id = flow_statements[flow_idx + 1]['id'] if flow_idx + 1 < len(flow_statements) else None
                    process_paragraph(session, statement['TrueStatements'], flow_statements, flow_idx, next_statement_id, true_path=True)
                    process_paragraph(session, statement['FalseStatements'], flow_statements, flow_idx, next_statement_id, true_path=False)                
                    
    driver.close()
    print("Δεδομένα στατικής ανάλυσης καταχωρήθηκαν επιτυχώς στη Neo4j!")

def process_paragraph(session, paragraph, flow_statements, flow_idx, next_statement_id, true_path=True):
    """
    Processes conditional statements within a flow and ensures that their **true** and **false paths** are correctly recorded in the database.
    
    Parameters:
        session: Neo4j session instance.
        paragraph (list): The block of statements inside a conditional branch.
        flow_statements (list): The main list of statements in the flow.
        flow_idx (int): The index of the current statement in the flow.
        next_statement_id (str): The ID of the statement to execute next.
        true_path (bool): Indicates whether this is the `TRUE_PATH` or `FALSE_PATH`.
    """    
    relationship = "TRUE_PATH" if true_path else "FALSE_PATH"
    for paragraph_statement in paragraph:
        session.execute_write(db_context.create_statement, paragraph_statement)
    for idx1, paragraph_statement in enumerate(paragraph):
        if (idx1 < len(paragraph) - 1):
            session.execute_write(db_context.create_relationship, paragraph_statement['id'], paragraph[idx1 + 1]['id'], 'EXECUTES')
        else:
            if flow_idx > len(flow_statements):
                print(f"Assosiatiaton {relationship} not created for {paragraph_statement['methodName']}")
                session.execute_write(db_context.create_relationship, paragraph_statement['id'], flow_statements[flow_idx + 1]['id'], 'NEXT')
            elif not is_condition(paragraph_statement):
                session.execute_write(db_context.create_relationship, paragraph_statement['id'], next_statement_id, 'NEXT')
        if is_condition(paragraph_statement):
            process_paragraph(session, paragraph_statement['TrueStatements'], paragraph, idx1, next_statement_id, true_path=True)
            process_paragraph(session, paragraph_statement['FalseStatements'], paragraph, idx1, next_statement_id, true_path=False)  
                           
    if paragraph:                   
        session.execute_write(db_context.create_relationship, flow_statements[flow_idx]['id'], paragraph[0]['id'], relationship)     
    else:                        
        if flow_idx + 1 < len(flow_statements):
            session.execute_write(db_context.create_relationship, flow_statements[flow_idx]['id'], flow_statements[flow_idx + 1]['id'], "NEXT")            


def is_condition(statement):
    return statement["type"] == "StatementType.CONDITION"




def generate_relationship(source_id, target_id, relation):
    return {"source_id": source_id, "target_id": target_id, "relation": relation}
    
if __name__ == "__main__":
    json_path = os.path.join(os.path.dirname(__file__),  "DOGEMAIN.json")
    print(json_path)
    with open(json_path) as f:
        data = json.load(f)

    process_json_data(json_path)
