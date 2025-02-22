import logging
import os
from neo4j import GraphDatabase
from static_analysis import logger

logger = logger.setup_logger('StaticAnalysisLogger', 'graph.log')

def create_program(tx, name, system, security):
    """Δημιουργία κόμβου Program με ασφαλή διαχείριση NULL τιμών"""
    logger.info(f"MERGE (p:Program [name: ${name}, system: ${system}, security: ${security}])")
    tx.run("""
        MERGE (p:Program {name: $name, system: $system, security: $security})
    """, name=name, system=system, security=security if security is not None else "")
    
def create_flow(tx, program_name, name, input, output):
    """Δημιουργία κόμβου Flow και σύνδεση με το Program"""
    logger.info(f"""
        MERGE (f:Flow [name: ${name}, input: ${input}, output: ${output}])
        WITH f
        MATCH (p:Program [name: ${program_name}])
        MERGE (p)-[:CONTAINS]->(f)
                """)
    tx.run("""
        MERGE (f:Flow {name: $name, input: $input, output: $output})
        WITH f
        MATCH (p:Program {name: $program_name})
        MERGE (p)-[:CONTAINS]->(f)
    """, program_name=program_name, name=name, input=str(input) if input else "{}", output=str(output) if output else "{}")
    
    
# Δημιουργία Statements (Conditional, Call, Assign)
def create_statement(tx, statement):
    # logger.info(f"statement={statement}")
    
    statement_type = statement["type"]
    # order = statement["order"]
    
    # Ανάλογα με το type, δημιουργούμε το σωστό node
    if statement_type == "StatementType.CONDITION":
        logger.info(f"MERGE (s:ConditionalStatement [id: ${statement["id"]}, methodName: ${statement.get("methodName", "")}])")
        query = "MERGE (s:ConditionalStatement {id: $id, methodName: $methodName})"
    elif statement_type == "StatementType.CALL":
        logger.info(f"MERGE (s:CallStatement [id: ${statement["id"]}, methodName: ${statement.get("methodName", "")}])")
        query = "MERGE (s:CallStatement {id: $id, methodName: $methodName})"
    elif statement_type == "StatementType.ASSIGN":
        logger.info(f"MERGE (s:AssignStatement [id: ${statement["id"]}, methodName: ${statement.get("methodName", "")}, assignFrom: ${statement.get("assignFrom", "")}, assignTo: ${statement.get("assignTo", "")}])")
        query = "MERGE (s:AssignStatement {id: $id, methodName: $methodName, assignFrom: $assignFrom, assignTo: $assignTo})"
    else:
        logger.info(f"MERGE (s:Statement [id: ${statement["id"]}, methodName: ${statement.get("methodName", "")}, type: ${statement.get("type", "")}")
        query = "MERGE (s:Statement {id: $id, methodName: $methodName, type: $type})"

    tx.run(query, id=statement["id"], methodName=statement.get("methodName", ""),
           assignFrom=statement.get("assignFrom", ""), assignTo=statement.get("assignTo", ""),
            type=statement_type)
    
    
def create_flow_to_statement_relationship(tx, flow_name, first_statement_id):
    logger.info(f"""
        MATCH (f:Flow [name: ${flow_name}]), (s [id: ${first_statement_id}])
        MERGE (f)-[:EXECUTES]->(s)
    """)
    tx.run("""
        MATCH (f:Flow {name: $flow_name}), (s {id: $first_statement_id})
        MERGE (f)-[:EXECUTES]->(s)
    """, flow_name=flow_name, first_statement_id=first_statement_id)
    
    
# Δημιουργία Relationships (EXECUTES, TRUE_PATH, FALSE_PATH, NEXT)
def create_relationship(tx, source_id, target_id, relationship_type):
    logger.info(f"""
        MATCH (a [id: ${source_id}]), (b [id: ${target_id}])
        MERGE (a)-[r:{relationship_type}]->(b)
    """)
    tx.run("""
        MATCH (a {id: $source_id}), (b {id: $target_id})
        MERGE (a)-[r:%s]->(b)
    """ % relationship_type, source_id=source_id, target_id=target_id)