import os
import sys
# import json
from antlr4 import FileStream, CommonTokenStream #, PredictionMode
# from custom_error_listener import CustomErrorListener
from grammars.Cobol85Lexer import Cobol85Lexer
from grammars.Cobol85Parser import Cobol85Parser
from parse_identification_division import parse_identification_division
from parse_procedure_division import parse_procedure_division
from parse_working_storage_section import parse_working_storage
from parse_linkage_section import parse_linkage_section
sys.path.append('../')
from logger import logger
# import time




def create_output_dir(output_dir="output"):
    """ Δημιουργεί τον φάκελο εξόδου αν δεν υπάρχει """
    os.makedirs(output_dir, exist_ok=True)
    return output_dir


# def parse_cobol_file(file_path):
#     """ Διαβάζει ένα COBOL αρχείο και δημιουργεί το συντακτικό του δέντρο """
#     logger.info(f"Processing file {file_path}...")
#     input_stream = FileStream(file_path, encoding="utf-8")
#     lexer = Cobol85Lexer(input_stream)
#     token_stream = CommonTokenStream(lexer)
#     parser = Cobol85Parser(token_stream)
#     # error_listener = CustomErrorListener()
#     # parser.removeErrorListeners()  # Αφαιρεί τον default listener
#     # parser.addErrorListener(error_listener)
#     start_time = time.time()
#     # parser.ERROR
#     # tree = parser.startRule()
#     # print("Processing took:", time.time() - start_time, "seconds")        
#     # if error_listener.errors:
#     #     for error in error_listener.get_errors():
#     #         logger.error(error)
#     # else:
#     #     print("Parsing completed successfully.")

#     return tree


# def analyze_tree(tree):
#     """ Εφαρμόζει τους visitors στο δέντρο και επιστρέφει την ανάλυση """
#     print(tree)
#     if tree:
#         static_analysis = parse_identification_division(tree)
#         variables = parse_working_storage_section.parse_working_storage(tree)
#         print(parse_working_storage_section.get_variables_json(variables))
#         procedure_analysis = parse_procedure_division(tree, static_analysis)  # Ανάλυση Procedure Division
#         # static_analysis.Flow = procedure_analysis.Flow
#         return static_analysis
#     else:
#         print("Error: No parse tree available.")
#         return None


def save_analysis_to_file(static_analysis, file_path, output_dir="output"):
    """ Αποθηκεύει τα αποτελέσματα της ανάλυσης σε JSON αρχείο """
    logger.info(f"Saving file {file_path}...")
    
    file_name = os.path.basename(file_path)
    file_name_without_ext = os.path.splitext(file_name)[0]
    output_file = os.path.join(output_dir, f"{file_name_without_ext}.json")

    json_output = static_analysis.to_json()

    with open(output_file, "w", encoding="utf-8") as f:
        f.write(json_output)

    print(f"Ανάλυση αποθηκεύτηκε στο: {output_file}")


def generate_diagrams(static_analysis):
    """ Δημιουργεί Flowchart και BPMN διαγράμματα """
    pass
    # if static_analysis:
        # diagram = FlowChartGenerator()
        # diagram.genrateDiagram(static_analysis, "RECEIVE-OPTION")


def process_cobol_file(file_path, output_dir="output"):
    """ Κύρια συνάρτηση που εκτελεί όλη τη διαδικασία ανάλυσης """
    logger.info(f"Processing file {file_path}...")
    input_stream = FileStream(file_path, encoding="utf-8")
    lexer = Cobol85Lexer(input_stream)
    token_stream = CommonTokenStream(lexer)
    parser = Cobol85Parser(token_stream)
    
    # error_listener = CustomErrorListener()
    # parser.removeErrorListeners()  # Αφαιρεί τον default listener
    # parser.addErrorListener(error_listener)    
    tree = parser.startRule()
    if tree:
        program_unit = tree.compilationUnit().programUnit(0)
        identification_division = program_unit.identificationDivision()
        static_analysis = parse_identification_division(program_unit.identificationDivision())
        workingStorage = find_section(Cobol85Parser.WorkingStorageSectionContext, program_unit.dataDivision())
        static_analysis = parse_working_storage(workingStorage, static_analysis)
        linkage = find_section(Cobol85Parser.LinkageSectionContext, program_unit.dataDivision())
        static_analysis = parse_linkage_section(linkage, static_analysis)
        procedure = program_unit.procedureDivision()
        static_analysis = parse_procedure_division(procedure, static_analysis)
        logger.debug(static_analysis)    
        # if error_listener.errors:
        #     for error in error_listener.get_errors():
        #         logger.error(error)    
        if static_analysis:
            save_analysis_to_file(static_analysis, file_path, output_dir)                
        return
        static_analysis = parse_identification_division(tree)
        variables = parse_working_storage_section.parse_working_storage(tree)
        print(parse_working_storage_section.get_variables_json(variables))
        procedure_analysis = parse_procedure_division(tree, static_analysis)  # Ανάλυση Procedure Division
        # static_analysis.Flow = procedure_analysis.Flow
        return static_analysis    
    
    
    
    # logger.debug(tree.toStringTree())

    # try:
    #     create_output_dir(output_dir)
    #     tree = parser.identificationDivision()
    #     if tree:
    #         static_analysis = parse_identification_division(tree)
    #     tree = parser.environmentDivision()       
    #     logger.debug(tree.toStringTree())
    #     tree = parser.dataDivision()     
    #     logger.debug(tree.toStringTree())
    #     tree = parser.workingStorageSection()
    #     logger.debug(tree.toStringTree())            
    #     if tree:
    #         static_analysis = parse_working_storage(ctx=tree, static_analysis=static_analysis)
    #     tree = parser.linkageSection()            
    #     if tree:
    #         static_analysis = parse_linkage_section.parse_linkage_section(ctx=tree, static_analysis=static_analysis)
    #     tree = parser.procedureDivision()            
    #     if tree:
    #         static_analysis = parse_procedure_division(tree, static_analysis)
    #     if static_analysis:
    #         save_analysis_to_file(static_analysis, file_path, output_dir)
    # except Exception as e:
    #     logger.error(f"Error processing file {file_path}: {e}")                
        

        
    #     # //return
    #     # Retrieve program information from Identification Division
    #     # tree = parser.identificationDivision()
    #     static_analysis = parse_identification_division(tree)
    #     # tree = parser.startRule()

    #     tree1 = parser.environmentDivision()
    #     # print(tree1.toStringTree())
        
    #     # retrive data structures from Working Storage Section    
    #     tree = parser.dataDivision()
    #     tree = parser.workingStorageSection()
    #     static_analysis = parse_working_storage(ctx=tree, static_analysis=static_analysis)
    #     logger.debug(static_analysis)
    #     # retrive data structures from Linkage Section    
    #     start_time = time.time()
    #     tree = parser.linkageSection()
    #     static_analysis = parse_linkage_section.parse_linkage_section(ctx=tree, static_analysis=static_analysis)
    #     print("Linkage parsing took:", time.time() - start_time, "seconds")

    #     start_time = time.time()
    #     tree = parser.procedureDivision()
    #     print("Parsing took:", time.time() - start_time, "seconds")
    #     start_time = time.time()
    #     if tree:
    #         # retrieve program flow from Procedure Division
    #         static_analysis = parse_procedure_division(tree, static_analysis)
    #     print("Processing took:", time.time() - start_time, "seconds")        
    # except Exception as e:
    #     logger.error(f"Error processing file {file_path}: {e}")
       

    
    #     generate_diagrams(static_analysis)
def find_section(section_class, ctx):
    """
    Αναζητά αναδρομικά το WorkingStorageSection σε ένα υποδέντρο του parse tree.
    """
    if ctx is None:
        return None

    # Αν ο κόμβος είναι ήδη WorkingStorageSection, επιστρέφουμε
    if isinstance(ctx, section_class):
        return ctx

    # Αναζητάμε αναδρομικά σε όλους τους υποκόμβους
    for i in range(ctx.getChildCount()):
        result = find_section(section_class, ctx.getChild(i))
        if result is not None:
            return result  # Βρήκαμε το WorkingStorageSection, σταματάμε την αναζήτηση

    return None  # Αν δεν βρέθηκε τίποτα
# test
process_cobol_file(".\\Samples\\DOGEMAIN.cbl")