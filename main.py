import argparse
import glob
import os
from parse_cobol import process_cobol_file
from logger import logger


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

    # args = parser.parse_args()
    # process_files(args.file_pattern)
    process_files(".\Samples\DOGE*.cbl")

if __name__ == "__main__":
    main()
