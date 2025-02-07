# parse_identification_division.py
import json
from logger import logger
from models.StaticAnalysis import StaticAnalysis
from grammars.Cobol85Visitor import Cobol85Visitor
from grammars.Cobol85Parser import Cobol85Parser
import re

class ParseIdentificationDivision(Cobol85Visitor):
    def __init__(self):
        self.staticAnalysis = StaticAnalysis()
        
    # --- Identification Division analysis ---
    def visitIdentificationDivision(self, ctx):
        # Δημιουργία αντικειμένου ανάλυσης
        static_analysis = StaticAnalysis()

        # Καλούμε τις `visit` μεθόδους και αποθηκεύουμε τις τιμές τους
        for child in ctx.children:
            if isinstance(child, Cobol85Parser.ProgramIdParagraphContext):
                static_analysis.ProgramId = self.visitProgramIdParagraph(child)
            if isinstance(child, Cobol85Parser.IdentificationDivisionBodyContext):
                # Τώρα πρέπει να ψάξουμε μέσα στο IdentificationDivisionBody
                for sub_child in child.children:
                    if isinstance(sub_child, Cobol85Parser.AuthorParagraphContext):
                        static_analysis.Author = self.visitAuthorParagraph(sub_child) 

                    if isinstance(sub_child, Cobol85Parser.InstallationParagraphContext):
                        static_analysis.Installation = self.visitInstallationParagraph(sub_child)

                    if isinstance(sub_child, Cobol85Parser.DateWrittenParagraphContext):
                        static_analysis.DateWritten = self.visitDateWrittenParagraph(sub_child)

                    if isinstance(sub_child, Cobol85Parser.SecurityParagraphContext):
                        static_analysis.Security = self.visitSecurityParagraph(sub_child)
        #logger.infostatic_analysis)
        self.staticAnalysis = static_analysis
        return static_analysis  # Επιστρέφουμε το αντικείμενο


    def visitProgramIdParagraph(self, ctx):
        logger.info(f"\t\t{ctx.__class__.__name__}")
        result = ""
        # Υποθέτουμε ότι ο τρίτος child (index 2) περιέχει το Program-ID
        if ctx.getChildCount() > 2:
            result = ctx.getChild(2).getText().strip()
        return result

    def visitAuthorParagraph(self, ctx):
        return self.getValueFromIdentificationLine(ctx.getText().strip())
      

    def visitInstallationParagraph(self, ctx):
        return self.getValueFromIdentificationLine(ctx.getText().strip())

    def visitDateWrittenParagraph(self, ctx):
        return self.getValueFromIdentificationLine(ctx.getText().strip())

    def visitDateCompiledParagraph(self, ctx):
        return self.getValueFromIdentificationLine(ctx.getText().strip())

    def visitSecurityParagraph(self, ctx):
        return self.getValueFromIdentificationLine(ctx.getText().strip())


    def getValueFromIdentificationLine(self, line):
        # Διαχωρίζουμε την πρώτη εμφάνιση της τελείας `.`
        parts = line.split(".", 1)  # Χωρίζουμε στο πρώτο `.`
        
        if len(parts) > 1:
            value = parts[1].strip()  # Παίρνουμε το δεξί μέρος μετά το πρώτο `.`
            value = value[:-1] if value.endswith(".") else value  # Αφαιρούμε την τελευταία τελεία
        else:
            value = ""

        return value
        
    def get_staticΑnalysis(self):
        return self.staticAnalysis

