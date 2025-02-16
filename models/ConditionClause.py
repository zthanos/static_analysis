from dataclasses import dataclass
from typing import List


@dataclass
class ConditionClause:
    """Represents a single condition clause inside a ConditionalStatement."""
    Left: str = ""
    Operator: str = ""
    Right: str = ""

    @property
    def Clause(self) -> str:
        """Dynamically generates the clause as 'Left Operator Right'."""
        return f"{self.Left} {self.Operator} {self.Right}".strip()
    @property
    def methodName(self) -> str:
        return f"{self.Left} {self.Operator} {self.Right}".strip()