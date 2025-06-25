from dataclasses import dataclass, field
from typing import List, Dict, Any, Optional
from models.Statement import Statement
from models.StatementType import StatementType


@dataclass
class DataOperationStatement(Statement):
    """
    Represents a data-operation statement, όπως:
      - SORT file ON KEY ...
      - INSPECT var TALLYING ...
      - INITIALIZE var1 var2 REPLACING ...
      - MERGE file USING ... GIVING ...
    """
    operation: str=""  # π.χ. "SORT", "INSPECT", "INITIALIZE", "MERGE"
    arguments: List[str] = field(default_factory=list)
    # γενικά options / clauses για κάθε operation
    parameters: Dict[str, Any] = field(default_factory=dict)
    # προαιρετικό πεδίο για πιο σύνθετες εντολές (π.χ. merge keys)
    extra: Optional[Dict[str, Any]] = None

    def __post_init__(self):
        self.type = StatementType.DATA_OPERATION

    def to_json(self):
        base = super().to_json()
        base.update({
            "operation": self.operation,
            "arguments": self.arguments,
            "parameters": self.parameters,
            "extra": self.extra,
        })
        return base  