import FlowAnalyzer
from antlr4 import FileStream, CommonTokenStream
from antlr4.tree.Trees import Trees
from CobolVisitor import CobolVisitor
from FlowAnalyzer import FlowAnalyzer
from PreprocessorVisitor import CustomPreprocessorVisitor
from grammars.Cobol85Lexer import Cobol85Lexer
from grammars.Cobol85Parser import Cobol85Parser
from grammars.Cobol85PreprocessorLexer import Cobol85PreprocessorLexer
from grammars.Cobol85PreprocessorParser import Cobol85PreprocessorParser

import json

class CobolParser:
    def __init__(self):
        self.tree = None
        self.visitor = CobolVisitor()
        self.flow_analyzer = FlowAnalyzer()
        
    def parse_file(self, file_path):
        input_stream = FileStream(file_path, encoding="utf-8")
        lexer = Cobol85Lexer(input_stream)
        token_stream = CommonTokenStream(lexer)
        parser = Cobol85Parser(token_stream)

        tree = parser.startRule()
        self.analyze(tree)
        # Εξαγωγή του tree σε DOT format
        # with open("parse_tree.dot", "w") as f:
        #     f.write(Trees.toStringTree(tree, recog=parser))
        
            
    def preprocess_and_parse_cobol(self, file_path):
        # Preprocessing
        input_stream = FileStream(file_path, encoding="utf-8")
        pre_lexer = Cobol85PreprocessorLexer(input_stream)
        pre_token_stream = CommonTokenStream(pre_lexer)
        pre_parser = Cobol85PreprocessorParser(pre_token_stream)
        pre_tree = pre_parser.startRule()

        visitor = CustomPreprocessorVisitor()
        visitor.visit(pre_tree)

        # Parsing
        lexer = Cobol85Lexer(FileStream(file_path, encoding="utf-8"))
        token_stream = CommonTokenStream(lexer)
        parser = Cobol85Parser(token_stream)
        tree = parser.startRule()
        self.analyze(tree)
        print("Parse Tree του COBOL Parser:")
        print(tree.toStringTree(recog=parser))


    def analyze(self, tree):
        """
        Εφαρμόζει τον visitor στο συντακτικό δέντρο και εκτυπώνει τα αποτελέσματα.
        """
        if tree:
            self.visitor.visit(tree)
            self.flow_analyzer.visitChildren(tree)
            self.print_results()
        else:
            print("Error: No parse tree available. Run parse_file() first.")

    def print_results(self):
        """
        Εκτυπώνει τα αποτελέσματα της ανάλυσης.
        """
        self.visitor.print_results()
        # Εκτύπωση του Flow Graph
        # self.flow_analyzer.print_flow()
        # print("Programs found:", self.visitor.programs)
        # print("Entry Points found:", self.visitor.entry_points)
        # print("Calls found:", self.visitor.calls)
        # print("Variables found:", self.visitor.variables)
        # print("Inputs per Entry Point:")
        # for entry, inputs in self.visitor.entry_inputs.items():
        #     print(f"  {entry}: {inputs}")                

# class CobolParserOld:
#     def __init__(self):
#         self.tree = None
#         self.visitor = CobolVisitor()
#         self.flow_analyzer = FlowAnalyzer()
        

#     def parse_file(self, file_path):
#         """
#         Διαβάζει και αναλύει ένα αρχείο COBOL.
#         """
#         try:
#             input_stream = FileStream(file_path, encoding='utf-8')
#             lexer = Cobol85Lexer(input_stream)
#             token_stream = CommonTokenStream(lexer)
#             parser = Cobol85Parser(token_stream)

#             # Ανίχνευση του σωστού root rule
#             self.tree = parser.startRule()  # Αν το σωστό root rule είναι διαφορετικό, άλλαξέ το!
#             # print(json.dumps(tree_str, indent=2))
#             print("Parsing completed successfully.")
#         except Exception as e:
#             print(f"Error while parsing: {e}")
            

    # def analyze(self):
    #     """
    #     Εφαρμόζει τον visitor στο συντακτικό δέντρο και εκτυπώνει τα αποτελέσματα.
    #     """
    #     if self.tree:
    #         self.visitor.visit(self.tree)
    #         self.flow_analyzer.visitChildren(self.tree)
    #         self.print_results()
    #     else:
    #         print("Error: No parse tree available. Run parse_file() first.")

    # def print_results(self):
    #     """
    #     Εκτυπώνει τα αποτελέσματα της ανάλυσης.
    #     """
    #     self.visitor.print_results()
    #     # Εκτύπωση του Flow Graph
    #     self.flow_analyzer.print_flow()
    #     # print("Programs found:", self.visitor.programs)
    #     # print("Entry Points found:", self.visitor.entry_points)
    #     # print("Calls found:", self.visitor.calls)
    #     # print("Variables found:", self.visitor.variables)
    #     # print("Inputs per Entry Point:")
    #     # for entry, inputs in self.visitor.entry_inputs.items():
    #     #     print(f"  {entry}: {inputs}")


