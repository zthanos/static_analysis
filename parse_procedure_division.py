import json
from models.Flow import Flow
from models.StaticAnalysis import StaticAnalysis
from grammars.Cobol85Parser import Cobol85Parser
from logger import logger
import context_info
import parse_statements


def parse_procedure_division(ctx, static_analysis):
    """
    Επισκέπτεται το PROCEDURE DIVISION και αναλύει το περιεχόμενο του.
    """
    logger.info("-------visit_procedure_division-----------")

    for child in context_info.get_children(ctx):
        if isinstance(child, Cobol85Parser.ProcedureDivisionBodyContext):
            logger.info("===================================================")
            visit_procedure_division_body(child, static_analysis)

    logger.info("-------End of ProcedureDivision-----------")
    return static_analysis

def visit_procedure_division_body(ctx, static_analysis):
    """
    Επισκέπτεται το ProcedureDivisionBody και αναλύει τις παραγράφους.
    """
    # logger.info("-------visit_procedure_division_body-----------")

    for child in context_info.get_children(ctx):
        if isinstance(child, Cobol85Parser.ParagraphsContext):
            visit_paragraphs_context(child, static_analysis)

def visit_paragraphs_context(ctx, static_analysis):
    """
    Επισκέπτεται τις παραγράφους μέσα στο PROCEDURE DIVISION.
    """
    # logger.info("-------visit_paragraphs_context-----------")

    for child in context_info.get_children(ctx):
        if isinstance(child, Cobol85Parser.ParagraphContext):
            visit_paragraph_context(child, static_analysis)

def visit_paragraph_context(ctx, static_analysis):
    """
    Επισκέπτεται μια παράγραφο και εξάγει τα δεδομένα της.
    """
    # logger.info("-------visit_paragraph_context-----------")

    flow = Flow()
    for child in context_info.get_children(ctx):
        if isinstance(child, Cobol85Parser.ParagraphNameContext):
            flow.Name = visit_paragraph_name_context(child)
        if isinstance(child, Cobol85Parser.SentenceContext):
            visit_sentence_context(child, flow)

    static_analysis.addFlow(flow)

def visit_paragraph_name_context(ctx):
    """
    Επιστρέφει το όνομα της παραγράφου.
    """
    # logger.info("-------visit_paragraph_name_context-----------")
    return ctx.getChild(0).getText()

def visit_sentence_context(ctx, flow):
    """
    Επισκέπτεται τις προτάσεις (sentences) μιας παραγράφου.
    """
    # logger.info("-------visit_sentence_context-----------")

    for child in context_info.get_children(ctx):
        if isinstance(child, Cobol85Parser.StatementContext):
            visit_statement_context(child, flow)

def visit_statement_context(ctx, flow):
    """
    Καλεί τον parser για statements και προσθέτει το αποτέλεσμα στη ροή.
    """
    statement = parse_statements.parse_statement(ctx)

    if statement is not None:
        flow.addSentence(statement)
