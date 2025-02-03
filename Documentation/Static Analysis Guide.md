# Legacy Code Static Analysis Guide
This document outlines the process of conducting static analysis on legacy code across different codebases. The goal is to extract meaningful insights, identify active and obsolete features, and map the application's flow for better understanding.

## **Process Overview**

### **1. Identify the Code Structure and Files**

- Determine the primary structure of the programming language(s) used in the legacy codebase.
- Identify the relevant file types that contain code, configurations, and logs necessary for the analysis.
- Understand the structure of log files and the information they contain.

### **2. Generate a Parser and Lexer**

- Use **ANTLR4** to generate a parser and lexer for the specific programming language.
- Ensure that the parser generates a **visitor class** in the target language to traverse and process the generated syntax tree.

### **3. Extract Critical Information**

- Implement methods to retrieve key data points such as:
    - Function definitions
    - Variable declarations
    - External API calls and integrations
    - Database interactions
- If production logs are available, analyze them to identify **active services** and **potentially abandoned features**.

### **4. Visualize Code Usage Patterns**

- Generate **heat maps** or **word clouds** to highlight the most frequently used functions, services, and modules.

### **5. Identify Key Entry Points**

- Scan the codebase to locate the **primary entry points**, prioritizing those that are most frequently used.
- Analyze the dependencies and connections between modules.

### **6. Map the Application Flow**

- Discover the execution flow starting from entry points.
- Track method calls, integrations, and data structure interactions.
- Document the discovered flow using **BPMN 2.0** by mapping service calls, dependencies, and decision paths.

By following this structured approach, you can gain valuable insights into a legacy codebase, identify areas for optimization, and facilitate migration or modernization efforts.