import json
from logger import logger
from models.AssignStatement import AssignStatement
from models.CallStatement import CallStatement
from models.Flow import Flow
from models.StaticAnalysis import StaticAnalysis
from grammars.Cobol85Visitor import Cobol85Visitor
from grammars.Cobol85Parser import Cobol85Parser
from models.ConditionalStatement import ConditionalStatement
from models.ConditionClause import ConditionClause
from helpers.ContextInfoHelper import ContextInfoHelper
from parsers.ParseConditionalStatements import ParseConditionalStatements



DEBUG_MODE = False  

def logger.info(message):
    if DEBUG_MODE:
        print(message)
        
class ParseProcedureDivision(Cobol85Visitor):
    """
    Visitor class for parsing the PROCEDURE DIVISION of a COBOL program.
    Extracts flows, statements, and method calls.
    """
    
    def __init__(self):
        """Initializes the visitor with an empty StaticAnalysis object."""
        self.staticAnalysis = StaticAnalysis()
        
    def visitProcedureDivision(self, ctx):
        """
        Visits the PROCEDURE DIVISION node in the parse tree.
        Identifies and processes the ProcedureDivisionBody.
        
        :param ctx: The parse tree context for ProcedureDivision.
        """
        logger.info("-------visitProcedureDivision-----------")
        for child in ContextInfoHelper.get_children(ctx):    
            if isinstance(child, Cobol85Parser.ProcedureDivisionBodyContext):            
                logger.info("===================================================")
                logger.info("")
                self.visitProcedureDivisionBody(child)
        # print(self.staticAnalysis.to_json())
        logger.info("-------End of ProcedureDivision-----------")       
       
    def visitProcedureDivisionBody(self, ctx):
        """
        Visits the ProcedureDivisionBody node.
        Identifies and processes Paragraphs.

        :param ctx: The parse tree context for ProcedureDivisionBody.
        """
        logger.info("-------visitProcedureDivisionBody-----------")
        for child in ContextInfoHelper.get_children(ctx):         
            if isinstance(child, Cobol85Parser.ParagraphsContext):
                self.visitParagraphsContext(child)

    def visitParagraphsContext(self, ctx):
        """
        Visits the Paragraphs node.
        Processes each Paragraph inside the PROCEDURE DIVISION.

        :param ctx: The parse tree context for Paragraphs.
        """
        logger.info("-------visitParagraphsContext-----------")
        for child in ContextInfoHelper.get_children(ctx):     
            if isinstance(child, Cobol85Parser.ParagraphContext):
                self.visitParagraphContext(child)
             
    def visitParagraphContext(self, ctx):            
        """
        Visits a Paragraph node.
        Extracts the paragraph name and processes its statements.

        :param ctx: The parse tree context for a Paragraph.
        """
        logger.info("-------visitParagraphContext-----------")
        
        flow = Flow()
        for child in ContextInfoHelper.get_children(ctx):     
            if isinstance(child, Cobol85Parser.ParagraphNameContext):
                flow.Name = self.visitParagraphNameContext(child)
            if isinstance(child, Cobol85Parser.SentenceContext):
                self.visitSentenceContext(child, flow)
        self.staticAnalysis.addFlow(flow)
                
    def visitParagraphNameContext(self, ctx):
        """
        Extracts and returns the name of a Paragraph.

        :param ctx: The parse tree context for ParagraphName.
        :return: The paragraph name as a string.
        """
        logger.info("-------visitParagraphNameContext-----------")
        
        return ctx.getChild(0).getText()
            
    def visitSentenceContext(self, ctx, flow):
        """
        Visits a Sentence node within a Paragraph.
        Processes each statement inside the sentence.

        :param ctx: The parse tree context for Sentence.
        :param flow: The Flow object to which statements are added.
        """
        logger.info("-------visitSentenceContext-----------")
        
        for child in ContextInfoHelper.get_children(ctx):     
            if isinstance(child, Cobol85Parser.StatementContext):
                self.visitStatementContext(child, flow)

    def visitStatementContext(self, ctx, flow):
        """
        Visits a Statement node.
        Processes MOVE and PERFORM statements.

        :param ctx: The parse tree context for Statement.
        :param flow: The Flow object to which the statement belongs.
        """
        logger.info("-------visitStatementContext-----------")
        
        for child in ContextInfoHelper.get_children(ctx):     
            if isinstance(child, Cobol85Parser.MoveStatementContext):
               flow.addSentence(self.visitMoveStatementContext(child))
            elif isinstance(child, Cobol85Parser.PerformStatementContext):
                flow.addSentence(self.visitPerformStatementContext(child))
            elif isinstance(child, Cobol85Parser.GobackStatementContext):
                flow.addSentence(self.visitGobackStatementContext(child))
            elif isinstance(child, Cobol85Parser.IfStatementContext):
                cond = self.visitIfStatementContext(child)
                print(cond)
                flow.addSentence(cond)

        
    def visitMoveStatementContext(self, ctx):
        """
        Visits a MoveStatement node.
        Extracts the assignment operation (MOVE A TO B → AssignFrom=A, AssignTo=B).

        :param ctx: The parse tree context for MoveStatement.
        :return: An AssignStatement object representing the MOVE operation.
        """
        logger.info("-------visitMoveStatementContext-----------")
        
        sentence = AssignStatement()
        for child in ContextInfoHelper.get_children(ctx):     
            if isinstance(child, Cobol85Parser.MoveToStatementContext):
                move_sentence = self.visitMoveToStatementContext(child)
                sentence.AssignFrom = move_sentence.AssignFrom
                sentence.AssignTo = move_sentence.AssignTo
        return sentence
            
    def visitMoveToStatementContext(self, ctx):
        """
        Visits a MoveToStatement node.
        Extracts the FROM and TO fields for an assignment operation.

        :param ctx: The parse tree context for MoveToStatement.
        :return: An AssignStatement object with AssignFrom and AssignTo set.
        """
        logger.info("-------visitMoveToStatementContext-----------")
        
        sentence = AssignStatement()
        for child in ContextInfoHelper.get_children(ctx):     
            if isinstance(child, Cobol85Parser.IdentifierContext):
                sentence.AssignTo = self.visitIdentifierContext(child)
            elif isinstance(child, Cobol85Parser.MoveToSendingAreaContext):
                sentence.AssignFrom = self.visitMoveToSendingAreaContext(child)
        return sentence
  
    def visitIdentifierContext(self, ctx):
        """
        Extracts and returns the identifier text.

        :param ctx: The parse tree context for Identifier.
        :return: The identifier text as a string.
        """
        logger.info("-------visitIdentifierContext-----------")
        
        return ctx.getChild(0).getText() if ctx.getChildCount() > 0 else ""

    def visitMoveToSendingAreaContext(self, ctx):
        """
        Extracts and returns the sending area for a MOVE statement.

        :param ctx: The parse tree context for MoveToSendingArea.
        :return: The sending area text as a string.
        """
        logger.info("-------visitMoveToSendingAreaContext-----------")
        
        return ctx.getChild(0).getText() if ctx.getChildCount() > 0 else ""

    def visitPerformStatementContext(self, ctx):
        """
        Visits a PerformStatement node.
        Extracts method calls (PERFORM statements).
        :param ctx: The parse tree context for PerformStatement.
        :return: A CallStatement object representing the PERFORM operation.
        """
        logger.info("-------visitPerformStatementContext-----------")

        for child in ContextInfoHelper.get_children(ctx):     
            # print(child.getText())
            if isinstance(child, Cobol85Parser.PerformProcedureStatementContext):
                return self.visitPerformProcedureStatementContext(child)
            elif isinstance(child, Cobol85Parser.PerformProcedureStatementContext):
                return self.visitPerformProcedureStatementContext(child)
            elif isinstance(child, Cobol85Parser.PerformInlineStatementContext):
                return self.visitPerformInlineStatementContext(child)
            # else:
            #     #log and return empry CallStatement
            #     ContextInfoHelper.printClassName(child)
            #     return CallStatement()
                
                

        return None  # If no valid child exists, return None
    
    def visitPerformInlineStatementContext(self, ctx):
        logger.info("-------visitPerformInlineStatementContext-----------")

        ContextInfoHelper.printClassName(ctx)
        print(ctx.getText())
        return CallStatement()        
    
    def visitPerformProcedureStatementContext(self, ctx):
        logger.info("-------visitPerformProcedureStatementContext-----------")
        
        ContextInfoHelper.printClassName(ctx)
        print(ctx.getText())
        return CallStatement()
            
    def visitPerformProcedureStatementContext(self, ctx):
        """
        Visits a PerformProcedureStatement node.
        Extracts and returns a CallStatement representing a PERFORM call.

        :param ctx: The parse tree context for PerformProcedureStatement.
        :return: A CallStatement object.
        """
        logger.info("-------visitPerformProcedureStatementContext-----------")
        
        callStatement = CallStatement()
        callStatement.MethodName = ctx.getText()
        return callStatement
    
    def visitGobackStatementContext(self, ctx):
        logger.info("-------visitGobackStatementContext-----------")
        
        callStatement = CallStatement()
        callStatement.MethodName = ctx.getText()
        return callStatement
    


    def visitIfStatementContext(self, ctx):
        logger.info("-------visitIfStatementContext-----------")
        conditionalParser = ParseConditionalStatements()

        for child in ContextInfoHelper.get_children(ctx):
            if isinstance(child, Cobol85Parser.IfThenContext):
                conditionalParser.visitIfThenContext(child)
            elif isinstance(child, Cobol85Parser.IfElseContext):
                conditionalParser.visitIfElseContext(child)

        return conditionalParser.conditionalStatement
    
    
    
    
    # # ✅ Επιστρέφουμε το conditional statement που έχει γεμίσει με τα σωστά δεδομένα
    # return conditionalStatement

    # # ----------------------------------------------------

    # def visitIfThenContext(self, ctx, conditionalStatement):
    #     logger.info("-------visitIfThenContext-----------")
        
    #     for child in ContextInfoHelper.get_children(ctx):    
    #         if isinstance(child, Cobol85Parser.CombinableConditionContext):
    #             condition = self.visitCombinableConditionContext(child)
    #             if condition:
    #                 conditionalStatement.addClause(condition)  # ✅ Προσθήκη της συνθήκης
    #         elif isinstance(child, Cobol85Parser.StatementContext):
    #             statement = self.visitStatementContext(child)
    #             if statement:
    #                 conditionalStatement.TrueStatements.append(statement)  # ✅ Προσθήκη στα True statements

    #     return conditionalStatement

    # # ----------------------------------------------------

    # def visitIfElseContext(self, ctx, conditionalStatement):
    #     logger.info("-------visitIfElseContext-----------")

    #     for child in ContextInfoHelper.get_children(ctx):     
    #         if isinstance(child, Cobol85Parser.StatementContext):
    #             statement = self.visitStatementContext(child)
    #             if statement:
    #                 conditionalStatement.FalseStatements.append(statement)  # ✅ Προσθήκη στα False statements

    #     return conditionalStatement

    # # ----------------------------------------------------

    # def visitCombinableConditionContext(self, ctx):
    #     logger.info("-------visitCombinableConditionContext-----------")

    #     for child in ContextInfoHelper.get_children(ctx):     
    #         if isinstance(child, Cobol85Parser.SimpleConditionContext):
    #             return self.visitSimpleConditionContext(child)  # ✅ Επιστρέφει ConditionClause

    #     return None  # Αν δεν βρει condition, επιστρέφει None

    # # ----------------------------------------------------

    # def visitSimpleConditionContext(self, ctx):
    #     logger.info("-------visitSimpleConditionContext-----------")

    #     conditionalStatement = ConditionalStatement()
    #     for child in ContextInfoHelper.get_children(ctx): 
    #         if isinstance(child, Cobol85Parser.RelationConditionContext):
    #             clause = self.visitRelationConditionContext(child)
    #             if clause is not None:
    #                 conditionalStatement.addClause(clause)
    #             else: 
    #                 logger.info("❌ Cannot create clause. Condition skipped")

    #     return conditionalStatement            

    # # ----------------------------------------------------

    # def visitRelationArithmeticComparisonContext(self, ctx):
    #     logger.info("-------visitRelationArithmeticComparisonContext-----------")
        
    #     return ConditionClause(
    #         Left=ContextInfoHelper.get_child_text(ctx, 0),
    #         Operator=ContextInfoHelper.get_child_concatenated_text(ctx, 1),
    #         Right=ContextInfoHelper.get_child_text(ctx, 2)
    #     )


    # # def visitIfStatementContext(self, ctx):
    # #     logger.info("-------visitIfStatementContext-----------")
    # #     # print(ctx.getText())
    # #     for child in ContextInfoHelper.get_children(ctx):     
    # #         if isinstance(child, Cobol85Parser.IfThenContext):
    # #             print("-------IfThenContext-----------")
    # #             # print(child.getText())
    # #             self.visitIfThenContext(child)
    # #         elif isinstance(child, Cobol85Parser.IfElseContext):
    # #             print("-------IfElseContext-----------")
    # #             print(child.getText())
                
    # #             # self.visitIfElseContext(child)

    # #         # else:
    # #         #     print("-------visitIfStatementContext-----------")
    # #         #     ContextInfoHelper.printClassName(child)
    # #         # if isinstance(child, Cobol85Parser.IfThenContext):
    # #         #     self.visitIfThenContext(child)
    # #         # if isinstance(child, Cobol85Parser.IfElseContext):        
    # #         #     self.visitIfElseContext(child)

            
        
    # # def visitIfThenContext(self, ctx):
    # #     logger.info("-------visitIfThenContext-----------")
    # #     print("-------visitIfThenContext-----------")
    # #     print(ctx.getText())
    # #     for child in ContextInfoHelper.get_children(ctx):    
    # #         ContextInfoHelper.printClassName(child) 
    # #         print(child.getText())
    # #         print(type(child))
    # #         if isinstance(child, Cobol85Parser.CombinableConditionContext):
    # #            self.visitCombinableConditionContext(child)
    # #         else:
    # #             print("-------ELSE visitIfThenContext-----------")
                
    # #             ContextInfoHelper.printClassName(child)


    # # def visitRelationConditionContext(self, ctx):
    # #     logger.info("-------visitRelationConditionContext-----------")
    # #     for child in ContextInfoHelper.get_children(ctx):     
    # #         if isinstance(child, Cobol85Parser.RelationArithmeticComparisonContext):
    # #             return self.visitRelationArithmeticComparisonContext(child)
        
        
        
        
    # # def visitIfElseContext(self, ctx):
    # #     logger.info("-------visitIfElseContext-----------")
        
    # #     for child in ContextInfoHelper.get_children(ctx):     
    # #         ContextInfoHelper.printClassName(child)

    # # def visitCombinableConditionContext(self, ctx):
    # #     logger.info("-------visitCombinableConditionContext-----------")
        
    # #     for child in ContextInfoHelper.get_children(ctx):     
    # #         if isinstance(child, Cobol85Parser.SimpleConditionContext):
    # #            return self.visitSimpleConditionContext(child)

                                
    
    # # def visitAndOrConditionContext(self, ctx):
    # #     logger.info("-------visitAndOrConditionContext-----------")
        
    # #     for child in ContextInfoHelper.get_children(ctx):     
    # #         ContextInfoHelper.printClassName(child)            

    # # def visitSimpleConditionContext(self, ctx):
    # #     logger.info("-------visitSimpleConditionContext-----------")
    # #     conditionalStatement = ConditionalStatement()
    # #     for child in ContextInfoHelper.get_children(ctx): 
    # #         if isinstance(child, Cobol85Parser.RelationConditionContext):
    # #             clause = self.visitRelationConditionContext(child)
    # #             if clause is not None:
    # #                 conditionalStatement.addClause(clause)
    # #             else: 
    # #                 logger.info("Cannot create clause. Condition skipped")
    # #         else:
    # #             ContextInfoHelper.printClassName(child)
    # #     return conditionalStatement            

    # # def visitRelationArithmeticComparisonContext(self, ctx):
    # #     logger.info("-------visitRelationArithmeticComparisonContext-----------")
    # #     return ConditionClause(Left=ContextInfoHelper.get_child_text(ctx, 0),
    # #                             Operator=ContextInfoHelper.get_child_concatenated_text(ctx, 1),
    # #                             Right=ContextInfoHelper.get_child_text(ctx, 2))

    # # def __get_children(self, ctx):
    # #     return list(ctx.getChildren())
    
    # # def __print_child(self, ctx):
    # #     logger.info(ctx.getText())
        
    # # def __get_child_text(self, ctx, idx):
    # #     return ctx.getChild(idx).getText()
        
    # # def __get_child_concatenated_text(self, ctx, idx):
    # #     """ Επιστρέφει το κείμενο όλων των child nodes ενωμένο με κενό (" "). """
    # #     ch = ctx.getChild(idx)
    # #     return " ".join(child.getText() for child in ch.getChildren())


    # # def __printClassName(self, obj):
    # #     """
    # #     Debugging utility: Prints the class name of an object.

    # #     :param obj: The object whose class name is to be printed.
    # #     """
    # #     print(obj.__class__.__name__)




# Supported Statement Contexts
# IfStatementContext
# ExecCicsStatementContext
# MultiplyStatementContext
# AddStatementContext
