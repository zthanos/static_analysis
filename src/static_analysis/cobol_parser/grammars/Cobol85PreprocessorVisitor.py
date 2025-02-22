# Generated from ./grammars/Cobol85Preprocessor.g4 by ANTLR 4.13.2
from antlr4 import *
if "." in __name__:
    from .Cobol85PreprocessorParser import Cobol85PreprocessorParser
else:
    from Cobol85PreprocessorParser import Cobol85PreprocessorParser

# This class defines a complete generic visitor for a parse tree produced by Cobol85PreprocessorParser.

class Cobol85PreprocessorVisitor(ParseTreeVisitor):

    # Visit a parse tree produced by Cobol85PreprocessorParser#startRule.
    def visitStartRule(self, ctx:Cobol85PreprocessorParser.StartRuleContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85PreprocessorParser#compilerOptions.
    def visitCompilerOptions(self, ctx:Cobol85PreprocessorParser.CompilerOptionsContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85PreprocessorParser#compilerXOpts.
    def visitCompilerXOpts(self, ctx:Cobol85PreprocessorParser.CompilerXOptsContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85PreprocessorParser#compilerOption.
    def visitCompilerOption(self, ctx:Cobol85PreprocessorParser.CompilerOptionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85PreprocessorParser#execCicsStatement.
    def visitExecCicsStatement(self, ctx:Cobol85PreprocessorParser.ExecCicsStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85PreprocessorParser#execSqlStatement.
    def visitExecSqlStatement(self, ctx:Cobol85PreprocessorParser.ExecSqlStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85PreprocessorParser#execSqlImsStatement.
    def visitExecSqlImsStatement(self, ctx:Cobol85PreprocessorParser.ExecSqlImsStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85PreprocessorParser#copyStatement.
    def visitCopyStatement(self, ctx:Cobol85PreprocessorParser.CopyStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85PreprocessorParser#copySource.
    def visitCopySource(self, ctx:Cobol85PreprocessorParser.CopySourceContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85PreprocessorParser#copyLibrary.
    def visitCopyLibrary(self, ctx:Cobol85PreprocessorParser.CopyLibraryContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85PreprocessorParser#replacingPhrase.
    def visitReplacingPhrase(self, ctx:Cobol85PreprocessorParser.ReplacingPhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85PreprocessorParser#replaceArea.
    def visitReplaceArea(self, ctx:Cobol85PreprocessorParser.ReplaceAreaContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85PreprocessorParser#replaceByStatement.
    def visitReplaceByStatement(self, ctx:Cobol85PreprocessorParser.ReplaceByStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85PreprocessorParser#replaceOffStatement.
    def visitReplaceOffStatement(self, ctx:Cobol85PreprocessorParser.ReplaceOffStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85PreprocessorParser#replaceClause.
    def visitReplaceClause(self, ctx:Cobol85PreprocessorParser.ReplaceClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85PreprocessorParser#directoryPhrase.
    def visitDirectoryPhrase(self, ctx:Cobol85PreprocessorParser.DirectoryPhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85PreprocessorParser#familyPhrase.
    def visitFamilyPhrase(self, ctx:Cobol85PreprocessorParser.FamilyPhraseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85PreprocessorParser#replaceable.
    def visitReplaceable(self, ctx:Cobol85PreprocessorParser.ReplaceableContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85PreprocessorParser#replacement.
    def visitReplacement(self, ctx:Cobol85PreprocessorParser.ReplacementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85PreprocessorParser#ejectStatement.
    def visitEjectStatement(self, ctx:Cobol85PreprocessorParser.EjectStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85PreprocessorParser#skipStatement.
    def visitSkipStatement(self, ctx:Cobol85PreprocessorParser.SkipStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85PreprocessorParser#titleStatement.
    def visitTitleStatement(self, ctx:Cobol85PreprocessorParser.TitleStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85PreprocessorParser#pseudoText.
    def visitPseudoText(self, ctx:Cobol85PreprocessorParser.PseudoTextContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85PreprocessorParser#charData.
    def visitCharData(self, ctx:Cobol85PreprocessorParser.CharDataContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85PreprocessorParser#charDataSql.
    def visitCharDataSql(self, ctx:Cobol85PreprocessorParser.CharDataSqlContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85PreprocessorParser#charDataLine.
    def visitCharDataLine(self, ctx:Cobol85PreprocessorParser.CharDataLineContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85PreprocessorParser#cobolWord.
    def visitCobolWord(self, ctx:Cobol85PreprocessorParser.CobolWordContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85PreprocessorParser#literal.
    def visitLiteral(self, ctx:Cobol85PreprocessorParser.LiteralContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85PreprocessorParser#filename.
    def visitFilename(self, ctx:Cobol85PreprocessorParser.FilenameContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Cobol85PreprocessorParser#charDataKeyword.
    def visitCharDataKeyword(self, ctx:Cobol85PreprocessorParser.CharDataKeywordContext):
        return self.visitChildren(ctx)



del Cobol85PreprocessorParser