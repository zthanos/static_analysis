import json
from dataclasses import dataclass, field, asdict
from typing import List, Dict
from models.Statement import Statement

@dataclass
class Flow:
    """
    Represents a COBOL PROCEDURE DIVISION flow.
    Contains a list of executed statements.
    """
    Name: str = ""
    Input: Dict = field(default_factory=dict)
    Output: Dict = field(default_factory=dict)
    Sentences: List["Statement"] = field(default_factory=list)

    def addSentence(self, sentence: "Statement"):
        """Adds a statement to the flow."""
        if isinstance(sentence, Statement):
            self.Sentences.append(sentence)
        else:
            raise TypeError(f"Invalid sentence type. Must be a Statement, got {type(sentence).__name__}.")

    def to_json(self) -> str:
        """Returns the JSON representation of the Flow."""
        return json.dumps(asdict(self), indent=4, default=str) 