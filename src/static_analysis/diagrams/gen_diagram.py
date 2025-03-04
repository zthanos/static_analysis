import os
import glob
import argparse

from logger import logger
from sequence import process_json_data

def create_output_dir(output_dir="output"):
    """ Δημιουργεί τον φάκελο εξόδου αν δεν υπάρχει """
    os.makedirs(output_dir, exist_ok=True)
    return output_dir

def save_diagram_to_file(static_analysis, file_path, output_dir="output"):
    """ Αποθηκεύει τα αποτελέσματα της ανάλυσης σε JSON αρχείο """
    logger.info(f"Saving file {file_path}...")
    
    file_name = os.path.basename(file_path)
    file_name_without_ext = os.path.splitext(file_name)[0]
    output_file = os.path.join(output_dir, f"{file_name_without_ext}.json")

    json_output = static_analysis.to_json()

    with open(output_file, "w", encoding="utf-8") as f:
        f.write(json_output)

    print(f"Ανάλυση αποθηκεύτηκε στο: {output_file}")

def process_files(file_pattern):
    """
    Βρίσκει και επεξεργάζεται αρχεία COBOL με βάση το file_pattern.
    
    :param file_pattern: Το όνομα αρχείου ή wildcard (π.χ. "*.cbl").
    """
    script_dir = os.path.dirname(os.path.abspath(__file__))
    pattern = os.path.join(script_dir, file_pattern)
    files = glob.glob(pattern)
    if not files:
        print(f"Δεν βρέθηκαν αρχεία που να ταιριάζουν με το μοτίβο: {file_pattern}")
        return

    for file_path in files:
        print(f"Επεξεργασία αρχείου: {file_path}")

def main():
    """
    Κεντρικός έλεγχος για την εκτέλεση του parser, λαμβάνοντας δύο arguments:
    - json_file: Το όνομα του αρχείου JSON προς επεξεργασία.
    - entry_point: Το entry point για την επεξεργασία.
    """
    parser = argparse.ArgumentParser(description="Static Analysis Sequence Diagram generator")
    
    # Ορισμός των arguments από τη γραμμή εντολών
    parser.add_argument("json_file", help="Όνομα αρχείου JSON προς επεξεργασία")
    parser.add_argument("entry_point", help="Το entry point για την επεξεργασία")

    try:
        args = parser.parse_args()
        # Κλήση της συνάρτησης επεξεργασίας με τα δύο arguments
        process_json_data(args.json_file, args.entry_point)
    except Exception as e:
        logger.error(f"Σφάλμα κατά την εκτέλεση: {e}")

if __name__ == "__main__":
    main()
