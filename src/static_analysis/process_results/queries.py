import sys
import os

# Βρες το path του "src" και πρόσθεσέ το στο sys.path
BASE_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), "../.."))
sys.path.append(BASE_DIR)

import json
from static_analysis.process_results import db_connection
from static_analysis import logger
import os
from neo4j import exceptions



driver = db_connection.get_neo4j_driver()

def fetch_program_flow(flow_name):
    query = """
    MATCH p=(program:Program)-[:CONTAINS]->(f:Flow {name: $flow_name})-[:EXECUTES|TRUE_PATH|FALSE_PATH|NEXT*]->(s)
    RETURN 
    """
    with driver.session() as session:
        result = session.run(query, flow_name=flow_name)
        return [record["p"] for record in result]
    
    

def get_execution_sequence_json(session, flow_name):
    query = """
    MATCH (p:Program)-[:CONTAINS]->(f:Flow {name: $flow_name})
    MATCH (f)-[:EXECUTES|NEXT|TRUE_PATH|FALSE_PATH*]->(s)
    WITH p, f, s
    OPTIONAL MATCH (s)-[r:EXECUTES|NEXT]->(next)
    WITH p, f, s, COLLECT({relationship_type: type(r), next_statement_id: next.id}) AS executed
    OPTIONAL MATCH (s)-[true_r:TRUE_PATH]->(true_next)
    WITH p, f, s, executed, COLLECT({relationship_type: type(true_r), next_statement_id: true_next.id}) AS true_path
    OPTIONAL MATCH (s)-[false_r:FALSE_PATH]->(false_next)
    RETURN 
        p.name AS program,
        f.name AS flow,
        COLLECT({
            statement_id: s.id,
            statement_name: s.methodName,
            statement_type: labels(s),
            executed: executed,
            true_path: true_path,
            false_path: COLLECT({relationship_type: type(false_r), next_statement_id: false_next.id})
        }) AS execution_flow
    """
    try:
            result = session.run(query, flow_name=flow_name)
            data = [record.data() for record in result]
            json_data = json.dumps(data, indent=4)
            return json_data
    except exceptions.Neo4jError as e:
        print(f"Neo4j Query Error: {e}")
        return json.dumps({"error": str(e)}, indent=4)
    except exceptions.ClientError as e:
        print(f"Client Error: {e}")
        return json.dumps({"error": str(e)}, indent=4)
    except exceptions.TransientError as e:
        print(f"Transient Error: {e}")
        return json.dumps({"error": str(e)}, indent=4)
    except exceptions.DatabaseError as e:
        print(f"Database Error: {e}")
        return json.dumps({"error": str(e)}, indent=4)
    except Exception as e:
        print(f"Unexpected Error: {e}")
        return json.dumps({"error": f"Unexpected Error: {str(e)}"}, indent=4)

def generate_diagrams(flow_data, output_file="flow_diagram.puml"):
    
    
    plantuml_content = "@startuml\n"
    nodes = set()
    relationships = set()
    for path in flow_data:
        for rel in path.relationships:
            start_node = rel.start_node
            end_node = rel.end_node
            rel_type = rel.type
            print(start_node.labels)
            if "Program" in start_node.labels:
                nodes.add(f"title {start_node["name"]}")
                nodes.add("|Program|")
                nodes.add(f":{start_node["name"]};")
                
            if "Flow" in start_node.labels:
                nodes.add(f"|#dee4e8|{start_node["name"]}|")
            
            if "ConditionalStatement" in end_node.labels:
                nodes.add(f"if ({end_node["methodName"]}) then (yes)")
            if "AssignStatement" in end_node.labels:
                nodes.add(f":{end_node["methodName"]};")
            if "CallStatement" in end_node.labels:
                nodes.add(f":{end_node["methodName"]};")
            if "Statement" in end_node.labels:
                nodes.add(f":{end_node["methodName"]};")                
                
            
                
           # Προσθέτουμε τους κόμβους
            # nodes.add(f'class {start_node["name"]} {{\n  type: {start_node.labels}\n}}\n')
            # nodes.add(f'class {end_node["name"]} {{\n  type: {end_node.labels}\n}}\n')

            # Προσθέτουμε τις σχέσεις
            # relationships.add(f'{start_node["name"]} --[{rel_type}]--> {end_node["name"]}\n')
          
    # Συνδυάζουμε το τελικό διάγραμμα
    plantuml_content += "\n".join(nodes) + "\n" # + "\n".join(relationships)
    plantuml_content += "\n@enduml"

    # Αποθήκευση στο αρχείο
    with open(output_file, "w") as f:
        f.write(plantuml_content)

    print(f"PlantUML diagram saved as {output_file}")  




def generate_plantuml_swimlane(paths, output_file="flow_diagram.puml"):
    """Generates a PlantUML swimlane diagram from a list of paths."""

    swimlanes = {}  # Store nodes by their program name
    relationships = []  # Store relationships between nodes

    for path in paths:
        nodes = path.nodes
        for i in range(len(nodes)):
            node = nodes[i]
            if "Program" in node.labels:
                program_name = node["name"]
                if program_name not in swimlanes:
                    swimlanes[program_name] = []
                if node not in swimlanes[program_name]:
                    swimlanes[program_name].append(node)

            if i > 0:
                prev_node = nodes[i - 1]
                relationships.append((prev_node, node))

    with open(output_file, "w") as f:
        f.write("@startuml\n")
        f.write("skinparam monochrome true\n")

        # Define swimlanes
        for program_name in swimlanes:
            f.write(f"partition {program_name} {{\n")
            for node in swimlanes[program_name]:
                node_id = node.element_id.split(":")[-1]
                node_label = node.get("methodName", node.get("name", "Unknown"))
                if "ConditionalStatement" in node.labels:
                    f.write(f"  diamond \"{node_label}\" as {node_id}\n")
                elif "AssignStatement" in node.labels:
                    f.write(f"  rectangle \"{node_label}\" as {node_id}\n")
                elif "CallStatement" in node.labels:
                    f.write(f"  rectangle \"{node_label}()\" as {node_id}\n")
                elif "Statement" in node.labels:
                    f.write(f"  rectangle \"{node_label}\" as {node_id}\n")
                else:
                    f.write(f"  rectangle \"{node_label}\" as {node_id}\n")

            f.write("}\n")

        # Define relationships
        for prev_node, node in relationships:
            prev_node_id = prev_node.element_id.split(":")[-1]
            node_id = node.element_id.split(":")[-1]

            # Determine relationship type
            relationship_type = ""
            for rel in paths[0].relationships:
              if rel.start_node == prev_node and rel.end_node == node:
                if "TRUE_PATH" in rel.type:
                  relationship_type = "-[#green]->"
                elif "FALSE_PATH" in rel.type:
                  relationship_type = "-[#red]->"
                elif "NEXT" in rel.type:
                  relationship_type = "-->"
                else:
                  relationship_type = "-->"
                break

            f.write(f"{prev_node_id} {relationship_type} {node_id}\n")

        f.write("@enduml\n")
    print(f"PlantUML diagram saved as {output_file}")



flow_data = get_execution_sequence_json(driver.session(), "00000-MAIN")
generate_diagrams(flow_data)
# generate_plantuml_swimlane(flow_data)

driver.close()

for data in flow_data:
    print(data)
    
