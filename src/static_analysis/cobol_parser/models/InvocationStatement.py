from dataclasses import dataclass, field
from typing import List
from models.Statement import Statement
from models.StatementType import StatementType

@dataclass    
class InvocationStatement(Statement):
    """Represents a COBOL PERFORM, CALL, or EXEC CICS statement."""
    Statements: List["Statement"] = field(default_factory=list)  # Υποστηρίζει sub-statements για CICS
    internal: bool = True
    
    def __post_init__(self):
        self.type = StatementType.INVOCATION

    @property
    def raw(self) -> str:
        """Returns the raw execution string."""
        call_type = "Perform" if self.internal else "Call"
        return f"{call_type} {self.methodName}"

    def addStatement(self, statement: "Statement"):
        """Adds a nested statement (for EXEC CICS commands)."""
        self.Statements.append(statement)