import argparse
import glob
import os

from logger import logger
from antlr4 import FileStream, CommonTokenStream
from grammars.Cobol85Lexer import Cobol85Lexer
from grammars.Cobol85Parser import Cobol85Parser
from parse_procedure_division import parse_procedure_division_section
from parse_identification_division import parse_identification_division_section
from parse_working_storage import parse_working_storage_section
from parse_linkage import parse_linkage_section


def process_files(file_pattern):
    """
    Βρίσκει και επεξεργάζεται αρχεία COBOL με βάση το file_pattern.
    
    :param file_pattern: Το όνομα αρχείου ή wildcard (π.χ. "*.cbl").
    """
    files = glob.glob(file_pattern) if not os.path.isfile(file_pattern) else [file_pattern]

    if not files:
        print(f"Δεν βρέθηκαν αρχεία που να ταιριάζουν με το μοτίβο: {file_pattern}")
        return

    for file_path in files:
        print(f"Επεξεργασία αρχείου: {file_path}")
        process_cobol_file(file_path)

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
        static_analysis = parse_identification_division_section(program_unit.identificationDivision())
        workingStorage = find_section(Cobol85Parser.WorkingStorageSectionContext, program_unit.dataDivision())
        static_analysis = parse_working_storage_section(workingStorage, static_analysis)
        linkage = find_section(Cobol85Parser.LinkageSectionContext, program_unit.dataDivision())
        static_analysis = parse_linkage_section(linkage, static_analysis)
        procedure = program_unit.procedureDivision()
        static_analysis = parse_procedure_division_section(procedure, static_analysis)
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


def main():
    """
    Ο κεντρικός έλεγχος για την εκτέλεση του parser, λαμβάνοντας input από το χρήστη.
    """
    parser = argparse.ArgumentParser(description="COBOL Static Analysis Parser")

    # Ορισμός argument για το όνομα αρχείου ή wildcard pattern
    parser.add_argument("file_pattern", help="Όνομα αρχείου COBOL ή wildcard pattern (π.χ. '*.cbl')")

    try:
        args = parser.parse_args()
        process_files(args.file_pattern)
    except Exception as e:
        logger.error(f"Σφάλμα κατά την εκτέλεση: {e}")


if __name__ == "__main__":
    main()
# process_cobol_file(".\\Samples\\DOGEMAIN.cbl")