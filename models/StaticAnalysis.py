import json
from dataclasses import dataclass, field, asdict
from typing import List, TYPE_CHECKING
from models.Flow import Flow

if TYPE_CHECKING:
    from models.Flow import Flow
    
@dataclass
class StaticAnalysis:
    ProgramId: str = ""
    Author: str = ""
    Installation: str = ""
    DateWritten: str = None
    Compiled: str = None
    Security: str = None
    Flow: List["Flow"] = field(default_factory=list) 

    def addFlow(self, flow: "Flow"):
        """Adds a Flow object to the Flow list."""
        if isinstance(flow, Flow):
            self.Flow.append(flow)
        else:
            raise TypeError(f"Invalid Flow type. Must be a Flow, got {type(flow).__name__}.")

    def to_json(self):
        """Serializes the object to JSON format."""
        return json.dumps(asdict(self), indent=4, default=str)
