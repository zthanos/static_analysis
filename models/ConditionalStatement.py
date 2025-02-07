from dataclasses import dataclass, field
from typing import List
from models.Statement import Statement
from models.ConditionClause import ConditionClause
from models.StatementType import StatementType

@dataclass
class ConditionalStatement(Statement):
    """Represents a conditional statement (IF condition) in COBOL code."""
    
    conditionClauses: List["ConditionClause"] = field(default_factory=list)
    TrueStatements: List["Statement"] = field(default_factory=list)
    FalseStatements: List["Statement"] = field(default_factory=list)
   
    def __post_init__(self):
        self.type = StatementType.CONDITION
        
    def addClause(self, clause):
        """Adds a condition clause to the statement."""
        if isinstance(clause, ConditionClause):
            self.conditionClauses.append(clause)
        else:
            raise TypeError(f"Expected ConditionClause, got {type(clause).__name__}.")

    def addTrueStatement(self, statement):
        """Adds a statement that executes when the condition is true."""
        if isinstance(statement, Statement):
            self.TrueStatements.append(statement)
        else:
            raise TypeError(f"Expected Statement, got {type(statement).__name__}.")

    def addFalseStatement(self, statement):
        """Adds a statement that executes when the condition is false."""
        if isinstance(statement, Statement):
            self.FalseStatements.append(statement)
        else:
            raise TypeError(f"Expected Statement, got {type(statement).__name__}.")
