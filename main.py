import argparse
import glob
import os
from CobolParser import CobolParser

def process_files(file_pattern):
    """
    Βρίσκει και επεξεργάζεται αρχεία COBOL με βάση το file_pattern.
    
    :param file_pattern: Το όνομα αρχείου ή wildcard (π.χ. "*.cbl").
    """
    # Αναζήτηση αρχείων που ταιριάζουν με το pattern
    files = glob.glob(file_pattern)

    if not files:
        print(f"Δεν βρέθηκαν αρχεία που να ταιριάζουν με το μοτίβο: {file_pattern}")
        return

    parser = CobolParser()
    
    for file_path in files:
        print(f"Επεξεργασία αρχείου: {file_path}")
        parser.parse_file(file_path)

def main():
    """
    Ο κεντρικός έλεγχος για την εκτέλεση του parser, λαμβάνοντας input από το χρήστη.
    """
    parser = argparse.ArgumentParser(description="COBOL Static Analysis Parser")

    # Ορισμός argument για το όνομα αρχείου ή wildcard pattern
    parser.add_argument("file_pattern", help="Όνομα αρχείου COBOL ή wildcard pattern (π.χ. '*.cbl')")

    # Ανάγνωση παραμέτρων από το CLI
    args = parser.parse_args()

    # Αν είναι πλήρες path ή απλά όνομα αρχείου
    if os.path.isfile(args.file_pattern):
        process_files(args.file_pattern)
    else:
        # Αν το μοτίβο δεν περιλαμβάνει path, αναζητά μόνο στον τρέχοντα φάκελο
        process_files(os.path.join(os.getcwd(), args.file_pattern))

if __name__ == "__main__":
    main()
