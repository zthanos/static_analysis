from enum import Enum, auto


class StatementType(Enum):

    ASSIGNMENT      = auto()
    INVOCATION      = auto()
    CONDITION       = auto()
    CICS            = auto()
    IO              = auto()
    CONTROL_TRANSFER= auto()
    DATA_OPERATION  = auto()
    LOOP            = auto()
    OTHER           = auto()