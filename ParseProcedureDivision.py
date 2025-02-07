import json
from parsers.ParseStatements import ParseStatements
from models.Flow import Flow
from models.StaticAnalysis import StaticAnalysis
from grammars.Cobol85Visitor import Cobol85Visitor
from grammars.Cobol85Parser import Cobol85Parser
from logger import logger
from helpers.ContextInfoHelper import ContextInfoHelper

       
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
        Delegates statement parsing to ParseStatements and adds the result to the flow.
        """
        statement_parser = ParseStatements()
        statement = statement_parser.visitStatementContext(ctx)

        if statement is not None:
            flow.addSentence(statement)




# Supported Statement Contexts
# ExecCicsStatementContext
# MultiplyStatementContext
# AddStatementContext
