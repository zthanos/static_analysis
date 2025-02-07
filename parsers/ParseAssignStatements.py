from logger import logger
from models.AssignStatement import AssignStatement
from helpers.ContextInfoHelper import ContextInfoHelper
from grammars.Cobol85Parser import Cobol85Parser

class ParseAssignStatements:
    """
    Parses MOVE statements inside PROCEDURE DIVISION.
    """

    def __init__(self):
        """Initializes the assignment statement parser."""
        pass

    def visitMoveStatementContext(self, ctx):
        """Parses a MOVE statement and returns an AssignStatement."""
        logger.info("-------visitMoveStatementContext-----------")
        sentence = AssignStatement()

        for child in ContextInfoHelper.get_children(ctx):
            if isinstance(child, Cobol85Parser.MoveToStatementContext):
                move_sentence = self.visitMoveToStatementContext(child)
                sentence.AssignFrom = move_sentence.AssignFrom
                sentence.AssignTo = move_sentence.AssignTo

        return sentence

    def visitMoveToStatementContext(self, ctx):
        """Extracts the assignment source and destination for a MOVE statement."""
        sentence = AssignStatement()

        for child in ContextInfoHelper.get_children(ctx):
            if isinstance(child, Cobol85Parser.IdentifierContext):
                sentence.AssignTo = self.visitIdentifierContext(child)
            elif isinstance(child, Cobol85Parser.MoveToSendingAreaContext):
                sentence.AssignFrom = self.visitMoveToSendingAreaContext(child)

        return sentence

    def visitIdentifierContext(self, ctx):
        """Returns the identifier text from the context."""
        return ctx.getChild(0).getText() if ctx.getChildCount() > 0 else ""

    def visitMoveToSendingAreaContext(self, ctx):
        """Returns the sending area text from the context."""
        return ctx.getChild(0).getText() if ctx.getChildCount() > 0 else ""
