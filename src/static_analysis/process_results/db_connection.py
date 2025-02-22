import os
from neo4j import GraphDatabase



# ⚠️ Βάλε τα στοιχεία της Neo4j Aura εδώ
NEO4J_URI = "neo4j+s://37866975.databases.neo4j.io"
USERNAME = "neo4j"
PASSWORD = "C7fZBkKEKGnoD5N2vqOgeWrXl3vsGR8cWbo3lTxivVI"

def get_neo4j_driver():
    return GraphDatabase.driver(NEO4J_URI, auth=(USERNAME, PASSWORD))
