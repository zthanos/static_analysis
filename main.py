from CobolParser import CobolParser
from CobolVisitor import CobolVisitor

# Δημιουργία ενός instance του CobolParser
parser = CobolParser()

# Ανάλυση ενός αρχείου COBOL

# parser.parse_file("./Samples/send.cbl")
#parser.parse_file("./Samples/Checkers.cbl")
# parser.parse_file("./Samples/RepWriteFull.cbl")
parser.parse_file("./Samples/Fibonacci Sequence.cbl")

parser.analyze()

