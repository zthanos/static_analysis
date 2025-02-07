from dataclasses import dataclass, field
from models.Statement import Statement
from models.StatementType import StatementType
from typing import List

@dataclass
class CallStatement(Statement):
    """Represents a COBOL PERFORM, CALL, or EXEC CICS statement."""
    MethodName: str = ""
    Internal: bool = True  # True = PERFORM, False = CALL
    Statements: List["Statement"] = field(default_factory=list)  # Υποστηρίζει sub-statements για CICS

    def __post_init__(self):
        self.type = StatementType.CALL

    @property
    def raw(self) -> str:
        """Returns the raw execution string."""
        call_type = "Perform" if self.Internal else "Call"
        return f"{call_type} {self.MethodName}"

    def addStatement(self, statement: "Statement"):
        """Adds a nested statement (for EXEC CICS commands)."""
        self.Statements.append(statement)
        
        
@dataclass
class CallCicsStatement(Statement):
    """Represents a COBOL EXEC CICS statement."""
    MethodName: str = ""  # Π.χ. READ, WRITE, DELETE
    Statements: List["Statement"] = field(default_factory=list)  # Υποστηρίζει sub-statements για CICS

    def __post_init__(self):
        self.type = StatementType.CICS

    @property
    def raw(self) -> str:
        """Returns the full EXEC CICS command."""
        params = " ".join(statement.raw for statement in self.Statements)
        return f"EXEC CICS {self.MethodName} {params}"

    def addStatement(self, statement: "Statement"):
        """Adds a nested statement (for EXEC CICS parameters)."""
        self.Statements.append(statement)
