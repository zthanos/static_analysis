from dataclasses import dataclass, field
import uuid
from typing import List, Dict, Any, Optional
from models.Statement import Statement
from models.StatementType import StatementType


@dataclass
class IOStatement(Statement):
    """
    Generic class for all COBOL I/O statements:
      - ACCEPT, DISPLAY
      - READ, WRITE
      - OPEN, CLOSE, START, etc.
    """
    # τί είδους I/O (π.χ. 'ACCEPT', 'READ', 'WRITE', ...)
    io_keyword: str = ""
    # το όνομα αρχείου ή μεταβλητής-στόχου (π.χ. fileName, identifier)
    target: str = ""
    # επιπλέον ορίσματα π.χ. λίστας offset, lock options, formatting κλπ.
    options: Dict[str, Any] = field(default_factory=dict)

    def __post_init__(self):
        # Θέτουμε το enum IO
        self.type = StatementType.IO

    def to_json(self):
        # Επαναχρησιμοποίησε το base JSON και πρόσθεσε I/O fields
        base = super().to_json()
        base.update({
            "io_keyword": self.io_keyword,
            "target": self.target,
            "options": self.options,
        })
        return base







    
