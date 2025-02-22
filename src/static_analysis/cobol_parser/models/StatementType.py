from enum import Enum


class StatementType(Enum):
    ASSIGN = "ASSIGN"
    CALL = "CALL"
    CONDITION = "CONDITION"
    CICS = "CICS"
    OTHER = "OTHER"
