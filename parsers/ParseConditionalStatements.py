from logger import logger
from models.ConditionalStatement import ConditionalStatement, ConditionClause
from helpers.ContextInfoHelper import ContextInfoHelper
from grammars.Cobol85Parser import Cobol85Parser

class ParseConditionalStatements:
    """
    Responsible for parsing conditional (IF) statements inside COBOL PROCEDURE DIVISION.
    """

    def visitIfStatementContext(self, ctx):
        """
        Parses an IF statement, extracting the condition and its associated statements.
        
        :param ctx: The parse tree context for IfStatement.
        :return: A fully constructed ConditionalStatement object.
        """
        logger.info("-------visitIfStatementContext-----------")

        conditionalStatement = ConditionalStatement()

        for child in ContextInfoHelper.get_children(ctx):     
            if isinstance(child, Cobol85Parser.IfThenContext):
                self.visitIfThenContext(child, conditionalStatement)
            elif isinstance(child, Cobol85Parser.IfElseContext):
                self.visitIfElseContext(child, conditionalStatement)

        return conditionalStatement

    # ----------------------------------------------------
    def visitIfThenContext(self, ctx, conditionalStatement):
        """
        Processes the IF-THEN condition and extracts true statements.

        :param ctx: The parse tree context for IfThen.
        :param conditionalStatement: The ConditionalStatement object being built.
        """
        logger.info("-------visitIfThenContext-----------")
        
        # Lazy Import μέσα στη μέθοδο για αποφυγή Circular Import
        from parsers.ParseStatements import ParseStatements
        parseStatements = ParseStatements()

        for child in ContextInfoHelper.get_children(ctx):
            if isinstance(child, Cobol85Parser.CombinableConditionContext):
                condition = self.visitCombinableConditionContext(child)
                if condition:
                    conditionalStatement.addClause(condition)  
            elif isinstance(child, Cobol85Parser.StatementContext):
                statement = parseStatements.visitStatementContext(child)
                if statement:
                    conditionalStatement.addTrueStatement(statement)

    # ----------------------------------------------------
    def visitIfElseContext(self, ctx, conditionalStatement):
        """
        Processes the ELSE condition and extracts false statements.

        :param ctx: The parse tree context for IfElse.
        :param conditionalStatement: The ConditionalStatement object being built.
        """
        logger.info("-------visitIfElseContext-----------")

        # Lazy Import μέσα στη μέθοδο για αποφυγή Circular Import
        from parsers.ParseStatements import ParseStatements
        parseStatements = ParseStatements()

        for child in ContextInfoHelper.get_children(ctx):
            if isinstance(child, Cobol85Parser.StatementContext):
                statement = parseStatements.visitStatementContext(child)
                if statement:
                    conditionalStatement.addFalseStatement(statement)

    # ----------------------------------------------------
    def visitCombinableConditionContext(self, ctx):
        """
        Extracts combinable condition clauses (e.g., AND, OR conditions).

        :param ctx: The parse tree context for CombinableCondition.
        :return: A ConditionClause object.
        """
        logger.info("-------visitCombinableConditionContext-----------")

        for child in ContextInfoHelper.get_children(ctx):
            if isinstance(child, Cobol85Parser.SimpleConditionContext):
                return self.visitSimpleConditionContext(child)

        return None  # Αν δεν βρει condition, επιστρέφει None

    # ----------------------------------------------------
    def visitSimpleConditionContext(self, ctx):
        """
        Extracts a simple condition (e.g., X > 5).

        :param ctx: The parse tree context for SimpleCondition.
        :return: A ConditionalStatement object.
        """
        logger.info("-------visitSimpleConditionContext-----------")

        conditionalStatement = ConditionalStatement()
        for child in ContextInfoHelper.get_children(ctx):
            if isinstance(child, Cobol85Parser.RelationConditionContext):
                clause = self.visitRelationConditionContext(child)
                if clause is not None:
                    conditionalStatement.addClause(clause)
                else:
                    logger.info("Cannot create clause. Condition skipped")

        return conditionalStatement

    # ----------------------------------------------------
    def visitRelationConditionContext(self, ctx):
        """
        Extracts a relational comparison condition (e.g., X = 5).

        :param ctx: The parse tree context for RelationCondition.
        :return: A ConditionClause object.
        """
        logger.info("-------visitRelationConditionContext-----------")

        for child in ContextInfoHelper.get_children(ctx):
            if isinstance(child, Cobol85Parser.RelationArithmeticComparisonContext):
                return self.visitRelationArithmeticComparisonContext(child)

    # ----------------------------------------------------
    def visitRelationArithmeticComparisonContext(self, ctx):
        """
        Extracts an arithmetic comparison condition (e.g., A > B).

        :param ctx: The parse tree context for RelationArithmeticComparison.
        :return: A ConditionClause object.
        """
        logger.info("-------visitRelationArithmeticComparisonContext-----------")

        return ConditionClause(
            Left=ContextInfoHelper.get_child_text(ctx, 0),
            Operator=ContextInfoHelper.get_child_concatenated_text(ctx, 1),
            Right=ContextInfoHelper.get_child_text(ctx, 2)
        )
