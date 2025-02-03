from antlr4 import  FileStream, CommonTokenStream
from PreprocessorVisitor import CustomPreprocessorVisitor
from grammars.Cobol85PreprocessorLexer import Cobol85PreprocessorLexer
from grammars.Cobol85PreprocessorParser import Cobol85PreprocessorParser

def preprocess_cobol(file_path):
    input_stream = FileStream(file_path, encoding="utf-8")
    lexer = Cobol85PreprocessorLexer(input_stream)
    token_stream = CommonTokenStream(lexer)
    parser = Cobol85PreprocessorParser(token_stream)
    tree = parser.startRule()

    visitor = CustomPreprocessorVisitor()
    visitor.visit(tree)

if __name__ == "__main__":
    preprocess_cobol("./Samples/DOGEMAIN.cbl")
