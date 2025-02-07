from logger import logger
from models.CallStatement import CallStatement, CallCicsStatement
from models.Statement import Statement
from helpers.ContextInfoHelper import ContextInfoHelper
from grammars.Cobol85Parser import Cobol85Parser

class ParseCallStatements:
    """
    Parses CALL and PERFORM statements inside PROCEDURE DIVISION.
    """

    def __init__(self):
        """Initializes the call statement parser."""
        pass

    def visitPerformStatementContext(self, ctx):
        """Parses a PERFORM statement and returns a CallStatement."""
        logger.info("-------visitPerformStatementContext-----------")

        for child in ContextInfoHelper.get_children(ctx):
            if isinstance(child, Cobol85Parser.PerformProcedureStatementContext):
                return self.visitPerformProcedureStatementContext(child)

        return None

    def visitPerformProcedureStatementContext(self, ctx):
        """Extracts the called procedure name from a PERFORM statement."""
        callStatement = CallStatement()
        callStatement.MethodName = ctx.getText()
        return callStatement

    def visitGobackStatementContext(self, ctx):
        """Parses a GOBACK statement and returns a CallStatement."""
        callStatement = CallStatement()
        callStatement.MethodName = "GOBACK"
        return callStatement
    
    def visitExecCicsStatementContext(self, ctx):
        """
        Processes EXEC CICS statements.
        These represent external system calls (DB access, messaging, transactions).
        """
        callStatement = CallCicsStatement()

        # Εντοπίζει την κύρια εντολή (π.χ., READ, WRITE, DELETE)
        for child in ContextInfoHelper.get_children(ctx):
            if isinstance(child, Cobol85Parser.ExecCicsCommandContext):
                command_name, params = self.visitExecCicsCommandContext(child)
                callStatement.MethodName = command_name  # Κύρια ενέργεια (READ, WRITE, DELETE)
                callStatement.Statements.extend(params)  # Τα υπόλοιπα arguments

        return callStatement

    def visitExecCicsCommandContext(self, ctx):
        """
        Processes individual CICS commands inside EXEC CICS.
        Example: EXEC CICS READ FILE("CUSTOMERS") INTO(DATA) END-EXEC.
        """
        ContextInfoHelper.print_class_name(ctx)

        children = ContextInfoHelper.get_children(ctx)
        if not children:
            return "", []

        command_name = children[0].getText()  # Πρώτη λέξη (π.χ., READ, WRITE, DELETE)
        params = [Statement(child.getText()) for child in children[1:]]  # Τα υπόλοιπα

        return command_name, params