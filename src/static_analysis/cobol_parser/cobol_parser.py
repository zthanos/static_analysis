import argparse
import glob
import os
from parse_cobol import process_cobol_file
from src.static_analysis.logger import logger
from process_results.process import process_json_data



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
