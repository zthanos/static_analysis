
# ANTLR4 - Lexer, Parser, and Visitor Class

When using **ANTLR4**, it generates three main components to process a language:

1. **Lexer**
    
    - Scans input text and breaks it into **tokens** (smallest meaningful units).
    - Uses **regular expressions** to define token patterns.
    - Converts raw text into a stream of tokens used by the parser.
2. **Parser**
    
    - Takes tokens from the lexer and organizes them according to the **grammar rules**.
    - Builds a **Parse Tree** that represents the syntactic structure of the code.
3. **Visitor Class**
    
    - Implements a **custom traversal** of the **Parse Tree**.
    - Extends `BaseVisitor<T>`, allowing you to define logic for visiting specific nodes.
    - Unlike a **Listener**, which automatically traverses the tree, the **Visitor** provides **explicit control**, letting you choose which nodes to visit and in what order.
