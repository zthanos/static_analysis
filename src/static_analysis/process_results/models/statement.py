from dataclasses import dataclass

@dataclass
class Statement:
    id: str
    methodName: str
    parentId: int
    level: int
    rate: int
    type: int
    previousStatementId: str = None
    nextStatementId: str = None    