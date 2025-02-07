from dataclasses import dataclass
from models.Statement import Statement
from models.StatementType import StatementType


@dataclass
class AssignStatement(Statement):
    """Represents a COBOL MOVE statement."""
    AssignFrom: str = ""
    AssignTo: str = ""

    def __post_init__(self):
        self.type = StatementType.ASSIGN

    @property
    def raw(self) -> str:
        """Returns the assignment operation in string format."""
        return f"{self.AssignFrom} = {self.AssignTo}"