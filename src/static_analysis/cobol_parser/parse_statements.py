from logger import logger
from grammars.Cobol85Parser import Cobol85Parser
import context_info
from models.Statement import Statement
from models.AssignStatement import AssignStatement 
from models.ConditionClause import ConditionClause
from models.ConditionalStatement import ConditionalStatement
from models.CallStatement import CallStatement, CallCicsStatement


def parse_statement(ctx):
    """
    Επισκέπτεται ένα Statement node και επιστρέφει το αντίστοιχο statement object.
    """
    # logger.info("-------visitStatementContext-----------")

    statement_map = {
        Cobol85Parser.MoveStatementContext: visit_move_statement_context,
        Cobol85Parser.PerformStatementContext: visit_perform_statement_context,
        Cobol85Parser.GobackStatementContext: visit_goback_statement_context,
        Cobol85Parser.IfStatementContext: visit_if_statement_context,
        Cobol85Parser.ExecCicsStatementContext: visit_exec_cics_statement_context
    }

    for child in context_info.get_children(ctx):
        for statement_type, visitor in statement_map.items():
            if isinstance(child, statement_type):
                return visitor(child)

    return None


# ----------------------------------------------------
# IF Statements
# ----------------------------------------------------

def visit_if_statement_context(ctx):
    """ Επεξεργάζεται ένα IF statement. """
    # logger.info("-------visitIfStatementContext-----------")
    conditional_statement = ConditionalStatement(methodName=f"IF {context_info.get_child_concatenated_text(ctx, 1)}")

    for child in context_info.get_children(ctx):
        if isinstance(child, Cobol85Parser.IfThenContext):
            visit_if_then_context(child, conditional_statement)
        elif isinstance(child, Cobol85Parser.IfElseContext):
            visit_if_else_context(child, conditional_statement)

    return conditional_statement


def visit_if_then_context(ctx, conditional_statement):
    """ Επεξεργάζεται το THEN μέρος του IF. """
    # logger.info("-------visitIfThenContext-----------")

    for child in context_info.get_children(ctx):
        if isinstance(child, Cobol85Parser.CombinableConditionContext):
            condition = visit_combinable_condition_context(child)
            if condition:
                conditional_statement.addClause(condition)
        elif isinstance(child, Cobol85Parser.StatementContext):
            statement = parse_statement(child)
            if statement:
                conditional_statement.addTrueStatement(statement)


def visit_if_else_context(ctx, conditional_statement):
    """ Επεξεργάζεται το ELSE μέρος του IF. """
    # logger.info("-------visitIfElseContext-----------")

    for child in context_info.get_children(ctx):
        if isinstance(child, Cobol85Parser.StatementContext):
            statement = parse_statement(child)
            if statement:
                conditional_statement.addFalseStatement(statement)


# ----------------------------------------------------
# Conditions
# ----------------------------------------------------

def visit_combinable_condition_context(ctx):
    """ Επεξεργάζεται συνδυασμένες συνθήκες (AND, OR). """
    # logger.info("-------visitCombinableConditionContext-----------")

    for child in context_info.get_children(ctx):
        if isinstance(child, Cobol85Parser.SimpleConditionContext):
            return visit_simple_condition_context(child)

    return None


def visit_simple_condition_context(ctx):
    """ Επεξεργάζεται μία απλή συνθήκη (π.χ. X > 5). """
    # logger.info("-------visitSimpleConditionContext-----------")
    conditional_statement = ConditionalStatement(methodName="IF")

    for child in context_info.get_children(ctx):
        if isinstance(child, Cobol85Parser.RelationConditionContext):
            clause = visit_relation_condition_context(child)
            if clause:
                conditional_statement.addClause(clause)

    return conditional_statement


def visit_relation_condition_context(ctx):
    """ Επεξεργάζεται συνθήκες σύγκρισης (π.χ. X = Y). """
    # logger.info("-------visitRelationConditionContext-----------")

    for child in context_info.get_children(ctx):
        if isinstance(child, Cobol85Parser.RelationArithmeticComparisonContext):
            return visit_relation_arithmetic_comparison_context(child)


def visit_relation_arithmetic_comparison_context(ctx):
    """ Επεξεργάζεται αριθμητικές συγκρίσεις (π.χ. A > B). """
    # logger.info("-------visitRelationArithmeticComparisonContext-----------")

    return ConditionClause(
        Left=context_info.get_child_text(ctx, 0),
        Operator=context_info.get_child_concatenated_text(ctx, 1),
        Right=context_info.get_child_text(ctx, 2)
    )


# ----------------------------------------------------
# MOVE Statements
# ----------------------------------------------------

def visit_move_statement_context(ctx):
    """ Επεξεργάζεται ένα MOVE statement και επιστρέφει ένα AssignStatement. """
    # logger.info("-------visitMoveStatementContext-----------")

    for child in context_info.get_children(ctx):
        if isinstance(child, Cobol85Parser.MoveToStatementContext):
            move_sentence = visit_move_to_statement_context(child)
            assignFrom = move_sentence.AssignFrom
            assignTo = move_sentence.AssignTo
            return AssignStatement(methodName=f"Assign value to {assignTo}", AssignFrom=assignFrom, AssignTo=assignTo)

    return None


def visit_move_to_statement_context(ctx):
    """ Επεξεργάζεται το MOVE TO μέρος ενός statement. """
    assignFrom = None
    assignTo = None
    for child in context_info.get_children(ctx):
        if isinstance(child, Cobol85Parser.IdentifierContext):
            assignTo = visit_identifier_context(child)
        elif isinstance(child, Cobol85Parser.MoveToSendingAreaContext):
            assignFrom = visit_move_to_sending_area_context(child)

    if assignFrom and assignTo:
        return AssignStatement(methodName=f"Assign value to {assignTo}", AssignFrom=assignFrom, AssignTo=assignTo)
    return None



def visit_identifier_context(ctx):
    """ Επιστρέφει το text του identifier. """
    return ctx.getChild(0).getText() if ctx.getChildCount() > 0 else ""


def visit_move_to_sending_area_context(ctx):
    """ Επιστρέφει το text του sending area. """
    return ctx.getChild(0).getText() if ctx.getChildCount() > 0 else ""


# ----------------------------------------------------
# PERFORM Statements
# ----------------------------------------------------

def visit_perform_statement_context(ctx):
    """ Επεξεργάζεται ένα PERFORM statement. """
    # logger.info("-------visitPerformStatementContext-----------")

    for child in context_info.get_children(ctx):
        if isinstance(child, Cobol85Parser.PerformProcedureStatementContext):
            return visit_perform_procedure_statement_context(child)

    return None


def visit_perform_procedure_statement_context(ctx):
    """ Επιστρέφει την διαδικασία που εκτελείται στο PERFORM. """
    return CallStatement(methodName=ctx.getText())


# ----------------------------------------------------
# EXEC CICS Statements
# ----------------------------------------------------

def visit_exec_cics_statement_context(ctx):
    """ Επεξεργάζεται ένα EXEC CICS statement. """
    # call_statement = CallStatement(False)

    for child in context_info.get_children(ctx):
        if isinstance(child, Cobol85Parser.ExecCicsCommandContext):
            call_statement = visit_exec_cics_command_context(child)
            # command_name, params = visit_exec_cics_command_context(child)
            # logger.info(f"----------------------------______{child.getText()}_______--------------------")
            # logger.info(params)
            # logger.info("----------------------------_________________________________________--------------------")            
            # call_statement.methodName = command_name
            # call_statement.Statements.extend(params)

            return call_statement


def visit_exec_cics_command_context(ctx):
    """ Επεξεργάζεται τις εντολές μέσα σε ένα EXEC CICS statement. """
    # context_info.print_class_name(ctx)
    children = context_info.get_children(ctx)
    if not children:
        return "", []
    call_statement = CallStatement(internal=False)    
    command_name = children[0].getText()
    logger.debug(f"command Name: {children[0].getText()}")
    call_statement.methodName = children[0].getText()
    params = [Statement(child.getText()) for child in children[1:]]
    for child in children[1:]:
        methodName = child.getText()
        call_statement.addStatement(CallStatement(False, methodName=methodName))
        # logger.debug(f"child: {child.getText()}")

    # logger.debug(call_statement)

    return call_statement
    # return command_name, params


# ----------------------------------------------------
# GOBACK Statement
# ----------------------------------------------------

def visit_goback_statement_context(ctx):
    """ Επεξεργάζεται ένα GOBACK statement. """
    return CallStatement(methodName="GOBACK")
