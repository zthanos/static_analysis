import json
import re
from logger import logger
from models.StaticAnalysis import StaticAnalysis
from grammars.Cobol85Parser import Cobol85Parser

def parse_identification_division(ctx):
    """
    Αναλύει το IDENTIFICATION DIVISION και επιστρέφει τα αποτελέσματα.
    """
    logger.info("Processing IDENTIFICATION DIVISION...")
    static_analysis = StaticAnalysis()

    for child in ctx.children:
        if isinstance(child, Cobol85Parser.ProgramIdParagraphContext):
            static_analysis.ProgramId = visit_program_id_paragraph(child)
        if isinstance(child, Cobol85Parser.IdentificationDivisionBodyContext):
            process_identification_body(child, static_analysis)

    return static_analysis

def process_identification_body(ctx, static_analysis):
    """
    Αναλύει το IdentificationDivisionBody και εξάγει πληροφορίες όπως Author, Installation, DateWritten, Security.
    """
    for sub_child in ctx.children:
        if isinstance(sub_child, Cobol85Parser.AuthorParagraphContext):
            static_analysis.Author = visit_author_paragraph(sub_child)
        elif isinstance(sub_child, Cobol85Parser.InstallationParagraphContext):
            static_analysis.Installation = visit_installation_paragraph(sub_child)
        elif isinstance(sub_child, Cobol85Parser.DateWrittenParagraphContext):
            static_analysis.DateWritten = visit_date_written_paragraph(sub_child)
        elif isinstance(sub_child, Cobol85Parser.SecurityParagraphContext):
            static_analysis.Security = visit_security_paragraph(sub_child)

def visit_program_id_paragraph(ctx):
    """
    Επιστρέφει το Program-ID από το αντίστοιχο context.
    """
    logger.info(f"\t\t{ctx.__class__.__name__}")
    return ctx.getChild(2).getText().strip() if ctx.getChildCount() > 2 else ""

def visit_author_paragraph(ctx):
    return extract_value_from_identification_line(ctx.getText().strip())

def visit_installation_paragraph(ctx):
    return extract_value_from_identification_line(ctx.getText().strip())

def visit_date_written_paragraph(ctx):
    return extract_value_from_identification_line(ctx.getText().strip())

def visit_security_paragraph(ctx):
    return extract_value_from_identification_line(ctx.getText().strip())

def extract_value_from_identification_line(line):
    """
    Εξάγει την πληροφορία μετά την πρώτη τελεία `.` στη γραμμή.
    """
    parts = line.split(".", 1)
    value = parts[1].strip() if len(parts) > 1 else ""
    return value[:-1] if value.endswith(".") else value
