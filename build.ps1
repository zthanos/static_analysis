pip install -r requirements.txt
antlr4 -Dlanguage=Python3 -visitor .\grammars\Cobol85.g4
antlr4 -Dlanguage=Python3 -visitor .\grammars\Cobol85Preprocessor.g4
