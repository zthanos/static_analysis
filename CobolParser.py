from antlr4 import *
from grammars.Cobol85Lexer import Cobol85Lexer
from grammars.Cobol85Parser import Cobol85Parser
from CobolVisitor import CobolVisitor

class CobolParser:
    def __init__(self):
        self.tree = None
        self.visitor = CobolVisitor()

    def parse_file(self, file_path):
        """
        Διαβάζει και αναλύει ένα αρχείο COBOL.
        """
        try:
            input_stream = FileStream(file_path, encoding='utf-8')
            lexer = Cobol85Lexer(input_stream)
            token_stream = CommonTokenStream(lexer)
            parser = Cobol85Parser(token_stream)

            # Ανίχνευση του σωστού root rule
            self.tree = parser.startRule()  # Αν το σωστό root rule είναι διαφορετικό, άλλαξέ το!
            print("Parsing completed successfully.")
        except Exception as e:
            print(f"Error while parsing: {e}")

    def analyze(self):
        """
        Εφαρμόζει τον visitor στο συντακτικό δέντρο και εκτυπώνει τα αποτελέσματα.
        """
        if self.tree:
            self.visitor.visit(self.tree)
            self.print_results()
        else:
            print("Error: No parse tree available. Run parse_file() first.")

    def print_results(self):
        """
        Εκτυπώνει τα αποτελέσματα της ανάλυσης.
        """
        self.visitor.print_results()
        # print("Programs found:", self.visitor.programs)
        # print("Entry Points found:", self.visitor.entry_points)
        # print("Calls found:", self.visitor.calls)
        # print("Variables found:", self.visitor.variables)
        # print("Inputs per Entry Point:")
        # for entry, inputs in self.visitor.entry_inputs.items():
        #     print(f"  {entry}: {inputs}")


