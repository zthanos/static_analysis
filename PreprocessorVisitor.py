from grammars.Cobol85PreprocessorVisitor import Cobol85PreprocessorVisitor
from grammars.Cobol85PreprocessorParser import Cobol85PreprocessorParser

class CustomPreprocessorVisitor(Cobol85PreprocessorVisitor):
    def visitCopyStatement(self, ctx: Cobol85PreprocessorParser.CopyStatementContext):
        print("Βρέθηκε COPY statement:", ctx.getText())
        return self.visitChildren(ctx)

    def visitReplaceStatement(self, ctx: Cobol85PreprocessorParser.ReplaceByStatementContext):
        print("Βρέθηκε REPLACE statement:", ctx.getText())
        return self.visitChildren(ctx)
