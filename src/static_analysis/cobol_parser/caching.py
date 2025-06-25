import pickle
from antlr4 import *
from YourLexer import YourLexer
from YourParser import YourParser

class SerializableNode:
    def __init__(self, symbol=None, rule_index=None, children=None):
        self.symbol = symbol
        self.rule_index = rule_index
        self.children = children if children is not None else []

    def __repr__(self):
        return f"SerializableNode(symbol={self.symbol}, rule_index={self.rule_index}, children={[child.rule_index if not isinstance(child, TerminalNode) else str(child.symbol) for child in self.children]})"

class SerializableTerminalNode:
    def __init__(self, symbol):
        self.symbol = symbol

    def __repr__(self):
        return f"SerializableTerminalNode(symbol={self.symbol})"

def serialize_tree(tree):
    if isinstance(tree, TerminalNode):
        return SerializableTerminalNode(tree.symbol)
    else:
        serializable_node = SerializableNode(symbol=tree.symbol, rule_index=tree.getRuleIndex())
        for child in tree.getChildren():
            serializable_node.children.append(serialize_tree(child))
        return serializable_node

def deserialize_tree(serializable_tree, rule_names):
    if isinstance(serializable_tree, SerializableTerminalNode):
        return TerminalNode(serializable_tree.symbol)
    else:
        node = ParserRuleContext(parent=None, invokingState=-1) # Dummy parent and state
        node.ruleIndex = serializable_tree.rule_index
        node.children = [deserialize_tree(child, rule_names) for child in serializable_tree.children]
        for child in node.children:
            child.parentCtx = node
        return node

def parse_and_persist_serializable_tree(input_text, output_filename="serializable_parse_tree.pkl", parser_class=YourParser, lexer_class=YourLexer):
    input_stream = InputStream(input_text)
    lexer = lexer_class(input_stream)
    token_stream = CommonTokenStream(lexer)
    parser = parser_class(token_stream)
    tree = parser.startRule()

    serializable_tree = serialize_tree(tree)

    try:
        with open(output_filename, "wb") as f:
            pickle.dump((serializable_tree, parser.ruleNames), f)
        print(f"Serializable parse tree saved to: {output_filename}")
        return tree # Return the original tree for immediate use if needed
    except Exception as e:
        print(f"Failed to cache parse tree: {e}")
        return None

def load_persisted_serializable_tree(filename="serializable_parse_tree.pkl", parser_class=YourParser, lexer_class=YourLexer):
    try:
        with open(filename, "rb") as f:
            serializable_tree, rule_names = pickle.load(f)
        loaded_tree = deserialize_tree(serializable_tree, rule_names)

        # You might need to re-establish the token stream and parser
        # if your visitor relies on them.
        # input_stream = InputStream("dummy") # You might need a dummy input
        # lexer = lexer_class(input_stream)
        # token_stream = CommonTokenStream(lexer)
        # parser = parser_class(token_stream)
        # loaded_tree.parser = parser # Assign the parser to the loaded tree if needed

        print("Serializable parse tree loaded.")
        return loaded_tree, rule_names
    except Exception as e:
        print(f"Failed to load cached parse tree: {e}")
        return None, None

# Example Usage:
input_code = "your input code here"
original_tree = parse_and_persist_serializable_tree(input_code)

if original_tree:
    loaded_tree, rule_names = load_persisted_serializable_tree()
    if loaded_tree:
        print(f"Type of loaded tree: {type(loaded_tree)}")
        # You can now use 'loaded_tree' with your visitor
        # visitor = YourVisitor()
        # walker = ParseTreeWalker()
        # walker.walk(visitor, loaded_tree)