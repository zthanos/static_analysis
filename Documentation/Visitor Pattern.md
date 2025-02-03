# Visitor Pattern
The **Visitor Pattern** is a behavioral design pattern that allows you to **separate an algorithm from the objects it operates on**. It enables adding new operations to a class hierarchy **without modifying existing classes**, following the **Open-Closed Principle**.

- **Key Idea:** Instead of embedding logic in objects, the pattern delegates it to a separate **visitor** class.
- **Structure:**
    - A **Visitor Interface** defines operations for different element types.
    - A **Concrete Visitor** implements these operations.
    - **Elements (Objects)** accept a visitor and call the appropriate method.
- **Usage in Parsing:** Commonly used in **syntax tree traversal**, where the visitor processes different node types without altering the tree structure.