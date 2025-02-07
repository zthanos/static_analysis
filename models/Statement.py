from dataclasses import dataclass
from models.StatementType import StatementType


@dataclass
class Statement:
    """Base class for all COBOL statements."""
    type: StatementType = StatementType.OTHER

    def to_json(self):
        return {
            "type": self.type.name  # 👈 Μετατρέπουμε το Enum σε string
        }