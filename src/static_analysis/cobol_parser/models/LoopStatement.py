from dataclasses import dataclass, field
from typing import List
from models.Statement import Statement
from models.StatementType import StatementType

@dataclass    
class LoopStatement(Statement):
    """Represents a COBOL Loop statement."""
    condition: str = ""  # Condition for the loop, if applicable
    times: int = 0  # Number of times to loop, if applicable
    Statements: List["Statement"] = field(default_factory=list)  
    internal: bool = True
    
    def __post_init__(self):
        self.type = StatementType.LOOP

    @property
    def raw(self) -> str:
        """Returns the raw execution string."""
        call_type = "Perform" if self.internal else "Call"
        return f"{call_type} {self.condition} {self.times} times" if self.times > 0 else f"{call_type} {self.condition}"

    def addStatement(self, statement: "Statement"):
        """Adds a nested statement (for EXEC CICS commands)."""
        self.Statements.append(statement)