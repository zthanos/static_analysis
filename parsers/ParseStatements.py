from logger import logger
from helpers.ContextInfoHelper import ContextInfoHelper
from grammars.Cobol85Parser import Cobol85Parser
from parsers.ParseCallStatements import ParseCallStatements
from parsers.ParseAssignStatements import ParseAssignStatements
from parsers.ParseConditionalStatements import ParseConditionalStatements

class ParseStatements:
    """
    Parses individual statements (MOVE, PERFORM, IF, etc.) inside PROCEDURE DIVISION.
    """

    def __init__(self):
        """Initializes the statement parser."""
        self.call_parser = ParseCallStatements()
        self.assign_parser = ParseAssignStatements()
        self.conditional_parser = ParseConditionalStatements()

    def visitStatementContext(self, ctx):
        """
        Visits a Statement node and returns the corresponding statement object.

        :param ctx: The parse tree context for Statement.
        :return: A statement object (AssignStatement, CallStatement, etc.), or None if unrecognized.
        """
        logger.info("-------visitStatementContext-----------")

        for child in ContextInfoHelper.get_children(ctx):
            if isinstance(child, Cobol85Parser.MoveStatementContext):
                return self.assign_parser.visitMoveStatementContext(child)
            elif isinstance(child, Cobol85Parser.PerformStatementContext):
                return self.call_parser.visitPerformStatementContext(child)
            elif isinstance(child, Cobol85Parser.GobackStatementContext):
                return self.call_parser.visitGobackStatementContext(child)
            elif isinstance(child, Cobol85Parser.IfStatementContext):
                return self.conditional_parser.visitIfStatementContext(child)
            elif isinstance(child, Cobol85Parser.ExecCicsStatementContext):
                return self.call_parser.visitExecCicsStatementContext(child)            

        return None  # Αν δεν υπάρχει αναγνωρίσιμο statement
