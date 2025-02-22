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
                    process_paragraph(session, statement['TrueStatements'], flow_statements, flow_idx, true_path=True)
                    process_paragraph(session, statement['FalseStatements'], flow_statements, flow_idx, true_path=False)                

        #     for idx, statement in enumerate(flow["Statements"]):
        #         next_statement = flow["Statements"][idx + 1] if idx < len(flow["Statements"]) - 1 else None
        #         previous_statement = flow["Statements"][idx - 1] if idx > 0 else None
        #         position = StatementPosition.MIDDLE
        #         if idx == len(flow["Statements"]) - 1:
        #             position = StatementPosition.LAST
        #         elif idx == 0:
        #             position = StatementPosition.FIRST
        #         # position = StatementPosition.FIRST if idx == 0 else StatementPosition.LAST if idx == len(flow["Statements"]) - 1 else StatementPosition.MIDDLE
        #         relationships.extend(process_statement(session, flow_name, statement, previous_statement, next_statement, position))

        # for relationship in relationships:
        #     session.execute_write(db_context.create_relationship, relationship['source_id'], relationship['target_id'], relationship['relation'])

    driver.close()
    print("Δεδομένα στατικής ανάλυσης καταχωρήθηκαν επιτυχώς στη Neo4j!")

def process_paragraph(session, paragraph, flow_statements, flow_idx, true_path=True):
    relationship = "TRUE_PATH" if true_path else "FALSE_PATH"
    for paragraph_statement in paragraph:
        session.execute_write(db_context.create_statement, paragraph_statement)
    for idx1, paragraph_statement in enumerate(paragraph):
        if (idx1 < len(paragraph) - 1):
            session.execute_write(db_context.create_relationship, paragraph_statement['id'], paragraph[idx1 + 1]['id'], 'EXECUTES')
        else:
            session.execute_write(db_context.create_relationship, paragraph_statement['id'], flow_statements[flow_idx + 1]['id'], 'NEXT')
    if paragraph:                   
        session.execute_write(db_context.create_relationship, flow_statements[flow_idx]['id'], paragraph[0]['id'], relationship)     
    else:                        
        if flow_idx + 1 < len(flow_statements):
            session.execute_write(db_context.create_relationship, flow_statements[flow_idx]['id'], flow_statements[flow_idx + 1]['id'], "NEXT")            


# def process_paragraph(session, flow_name, paragraph, statement, previous_statement, next_statement, true_path=True):
#     relationships = []
#     relationship = "TRUE_PATH" if true_path else "FALSE_PATH"
#     ns = paragraph[0] if paragraph else next_statement
#     relationships.append(generate_relationship(statement["id"], ns["id"], relationship))     
#     prev = None        
#     for idx, st in enumerate(paragraph):
#         if idx > 0 and idx <= len(paragraph) -1:
#             relationships.append(generate_relationship(paragraph[idx - 1]["id"], statement["id"], "EXECUTES"))    
#         # previous_paragraph_statement = paragraph[idx - 1] if idx > 0 else statement
#         # next_paragraph_statement = paragraph[idx + 1] if idx < len(paragraph) - 1 else next_statement
#         # position = StatementPosition.MIDDLE
#         # if idx == len(paragraph) - 1:
#         #     position = StatementPosition.LAST
#         # elif idx == 0:
#         #     position = StatementPosition.FIRST
#         # prev = st['id']
#         # relationships.extend(process_statement(session, flow_name, st, previous_statement, next_paragraph_statement, position))
#     return relationships
          


# def process_statement(session, flow_name, statement, previous_statement, next_statement, statement_position):
#     relationships = []
#     session.execute_write(db_context.create_statement, statement)
#     match statement_position:
#         case StatementPosition.FIRST:
#             pass
#             # session.execute_write(db_context.create_flow_to_statement_relationship, flow_name, statement["id"])
#         case StatementPosition.LAST:
#             if next_statement:
#             #    relationships.append(generate_relationship(statement["id"], next_statement["id"], "NEXT"))    
#                 pass
#         case _:
#             if statement["type"] != "StatementType.CONDITION":
#                 pass
#                 # relationships.append(generate_relationship(statement["id"], next_statement["id"], "EXECUTES"))                          
#     #                 logger.info(f"EXECUTES for {statement["id"]}")
#     #             # else:
#     #             #     relationships.append(generate_relationship(previous_statement["id"], statement["id"], "EXECUTES"))  
#     if statement["type"] == "StatementType.CONDITION":
#         relationships.extend(process_paragraph(session, flow_name, statement["TrueStatements"], statement, previous_statement, next_statement, True))
#         relationships.extend(process_paragraph(session, flow_name, statement["FalseStatements"], statement,  previous_statement, next_statement, False))                  
#     return relationships
        



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
