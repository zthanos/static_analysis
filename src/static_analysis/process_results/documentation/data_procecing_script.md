# Static Analysis Data Processing Script

## Overview
This script processes static analysis data extracted from a COBOL program and stores the structured relationships in a **Neo4j** graph database. It parses a JSON file containing program flows, statements, and their relationships, and builds a graph representation for further analysis.

---

## Dependencies
- `neo4j`: Used to connect and interact with the Neo4j database.
- `gremlin-python`: If switching to **CosmosDB Gremlin**, Gremlin queries need to be used instead of Cypher.
- `json`: Handles reading and parsing JSON input data.
- `logging`: Captures logs related to data processing.
- `os`, `sys`: Handles file paths and module imports.

---

## Configuration
The script dynamically determines the base directory and ensures the necessary modules are accessible.

```python
# Determine the base directory and add it to sys.path
BASE_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), "../.."))
sys.path.append(BASE_DIR)
```

It also sets up a logger for tracking operations:

```python
logger = logger.setup_logger('StaticAnalysisLogger', 'static_analysis.log')
```

---

## Enum Definitions
An enumeration is defined to represent the position of a statement within a flow.
```python
class StatementPosition(Enum):
    FIRST = "first"
    MIDDLE = "middle"
    LAST = "last"
```

---

## Function: `process_json_data(json_file)`
### **Purpose**
Parses a JSON file containing program flow information and inserts it into the Neo4j database.

### **Parameters**
- `json_file (str)`: Path to the JSON file containing static analysis data.

### **Processing Steps**
1. Opens and reads the JSON file.
2. Retrieves a Neo4j driver session.
3. Extracts the **program name**, **flows**, and **statements**.
4. Inserts **program nodes** into the database.
5. Iterates through flows, inserting **flow nodes** and their corresponding **statements**.
6. Establishes relationships between statements based on execution order and conditional branching.

```python
with driver.session() as session:
    session.execute_write(db_context.create_program, program_name, system, security)
    for flow in data["Flows"]:
        session.execute_write(db_context.create_flow, program_name, flow_name, input, output)
        for flow_idx, statement in enumerate(flow_statements):
            session.execute_write(db_context.create_statement, statement)
```

### **Logging Output**
At the end of execution, a success message is printed:
```python
print("Static analysis data successfully stored in Neo4j!")
```

---

## Function: `process_paragraph(session, paragraph, flow_statements, flow_idx, next_statement_id, true_path=True)`
### **Purpose**
Processes conditional statements within a flow and ensures that their **true** and **false paths** are correctly recorded in the database.

### **Parameters**
- `session`: Neo4j session instance.
- `paragraph (list)`: The block of statements inside a conditional branch.
- `flow_statements (list)`: The main list of statements in the flow.
- `flow_idx (int)`: The index of the current statement in the flow.
- `next_statement_id (str)`: The ID of the statement to execute next.
- `true_path (bool)`: Indicates whether this is the `TRUE_PATH` or `FALSE_PATH`.

### **Processing Steps**
1. Iterates over statements within a condition’s block.
2. Creates relationships between consecutive statements (`EXECUTES`).
3. Ensures that conditional statements properly branch to `TRUE_PATH` and `FALSE_PATH`.

```python
if is_condition(paragraph_statement):
    process_paragraph(session, paragraph_statement['TrueStatements'], paragraph, idx1, next_statement_id, true_path=True)
    process_paragraph(session, paragraph_statement['FalseStatements'], paragraph, idx1, next_statement_id, true_path=False)
```

---

## Function: `is_condition(statement)`
### **Purpose**
Checks whether a statement is a conditional statement.

### **Implementation**
```python
def is_condition(statement):
    return statement["type"] == "StatementType.CONDITION"
```

---

## Function: `generate_relationship(source_id, target_id, relation)`
### **Purpose**
Creates a dictionary representing a relationship between two statements.

### **Implementation**
```python
def generate_relationship(source_id, target_id, relation):
    return {"source_id": source_id, "target_id": target_id, "relation": relation}
```

---

## Execution
When the script is run directly, it reads `DOGEMAIN.json` and processes it.

```python
if __name__ == "__main__":
    json_path = os.path.join(os.path.dirname(__file__), "DOGEMAIN.json")
    process_json_data(json_path)
```

---

## **Next Steps: Adapting to CosmosDB Gremlin**
If switching from **Neo4j** to **CosmosDB Gremlin**, the following changes are needed:

- **Modify queries from Cypher to Gremlin** (e.g., use `g.addV().property()` instead of `MERGE`).
- **Use the Gremlin Python client**:
  ```python
  from gremlin_python.driver import client
  gremlin_client = client.Client("wss://your-cosmosdb.gremlin.cosmos.azure.com:443/", "g")
  ```
- **Replace `session.execute_write()` calls with `gremlin_client.submit()`** for creating nodes and edges.

---

## Conclusion
This script successfully transforms static analysis JSON data into a graph model in Neo4j, handling program flows, statements, and relationships. Future enhancements could include:
- Migrating to **CosmosDB Gremlin** for cloud scalability.
- Adding **graph queries** to extract insights from the stored program structures.

🚀 **Ready for the next phase: Graph querying!**

