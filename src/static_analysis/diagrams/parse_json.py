from logger import logger

def get_flow(data, flow_name):
    main_node = next((node for node in data.get("Flow", []) if node.get("Name") == flow_name), None)
    if main_node:
        return main_node
    else:
        logger.info(f"Node '{flow_name}' not found.")
    return None

def is_condition(statement):
    return statement.get("type") == "StatementType.CONDITION"

def is_assignment(statement):
    return statement.get("type") == "StatementType.ASSIGN"

def is_internal_call(statement):
    return statement.get('type') == "StatementType.CALL" and statement.get("internal") == True

def is_external_call(statement):
    return statement.get('type') == "StatementType.CALL" and statement.get("internal") == False