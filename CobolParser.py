import os
import json
import FlowAnalyzer
from antlr4 import FileStream, CommonTokenStream
from IdentificationDivision import ParseIdentificationDivision
from CobolVisitor import CobolVisitor
from FlowAnalyzer import FlowAnalyzer
from grammars.Cobol85Lexer import Cobol85Lexer
from grammars.Cobol85Parser import Cobol85Parser
from FlowChartGenerator import FlowChartGenerator
from ParseProcedureDivision import ParseProcedureDivision

class CobolParser:
    def __init__(self, output_dir="output"):
        self.tree = None
        self.visitor = CobolVisitor()
        self.flow_analyzer = FlowAnalyzer()
        self.output_dir = output_dir  # Καθορισμός φακέλου εξόδου

        # Δημιουργία του output directory αν δεν υπάρχει
        os.makedirs(self.output_dir, exist_ok=True)

    def parse_file(self, file_path):
        input_stream = FileStream(file_path, encoding="utf-8")
        lexer = Cobol85Lexer(input_stream)
        token_stream = CommonTokenStream(lexer)
        parser = Cobol85Parser(token_stream)
        tree = parser.startRule()
        static_analysis = self.analyze(tree)
        diagram = FlowChartGenerator()
        diagram.genrateDiagram(static_analysis, "RECEIVE-OPTION")
        # print(static_analysis)
        if static_analysis:
            self.save_analysis_to_file(static_analysis, file_path)

    def analyze(self, tree):
        """
        Εφαρμόζει τον visitor στο συντακτικό δέντρο και επιστρέφει τα αποτελέσματα της ανάλυσης.
        """
        if tree:
            pid = ParseIdentificationDivision()
            pd = ParseProcedureDivision()
            pid.visit(tree)
            
            # Εξαγωγή δεδομένων από τα visitors
            static_analysis = pd.visit(tree)
            pd.visit(tree)  # Ανάλυση Procedure Division
            
            pid.staticAnalysis.Flow = pd.staticAnalysis.Flow
            return pid.staticAnalysis  # Επιστροφή των αποτελεσμάτων για αποθήκευση
        else:
            print("Error: No parse tree available. Run parse_file() first.")
            return None

    def save_analysis_to_file(self, static_analysis, file_path):
        """
        Αποθηκεύει τα αποτελέσματα της ανάλυσης σε JSON αρχείο μέσα στον output folder.

        :param static_analysis: Το αντικείμενο ανάλυσης που περιέχει τα δεδομένα.
        :param file_path: Το αρχικό COBOL αρχείο από το οποίο προήλθαν τα δεδομένα.
        """
        # Εξαγωγή του ονόματος του αρχείου από το file_path
        file_name = os.path.basename(file_path)
        file_name_without_ext = os.path.splitext(file_name)[0]
        output_file = os.path.join(self.output_dir, f"{file_name_without_ext}.json")

        # Μετατροπή των δεδομένων σε JSON
        json_output = static_analysis.to_json()

        # Αποθήκευση στο αρχείο
        with open(output_file, "w", encoding="utf-8") as f:
            f.write(json_output)

        print(f"Ανάλυση αποθηκεύτηκε στο: {output_file}")
