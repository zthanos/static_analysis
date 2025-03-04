from dataclasses import dataclass, field
import uuid
from models.StatementType import StatementType


@dataclass
class Statement:
    """Base class for all COBOL statements."""
    id: str = field(default_factory=lambda: str(uuid.uuid4()))
    internal: bool = True
    type: StatementType = StatementType.OTHER
    methodName: str = ""
    def to_json(self):
        return {
            "id": self.id,
            "type": self.type.name,
            "internal": self.internal 
        }