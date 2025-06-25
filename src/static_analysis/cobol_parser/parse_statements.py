from logger import logger
from grammars.Cobol85Parser import Cobol85Parser
import context_info
from models.Statement import Statement
from models.AssignStatement import AssignStatement 
from models.ConditionClause import ConditionClause
from models.ConditionalStatement import ConditionalStatement
from models.CallStatement import CallStatement, CallCicsStatement
from models.IOStatement import IOStatement
from models.DataOperationStatement import DataOperationStatement
from models.InvocationStatement import InvocationStatement
from models.LoopStatement import LoopStatement
import re


def parse_statement(ctx):
    """
    Επισκέπτεται ένα Statement node και επιστρέφει το αντίστοιχο statement object.
    """
    statement_map = {
        # Cobol85Parser.MoveStatementContext: visit_move_statement_context,
        Cobol85Parser.MoveStatementContext: process_assigment_statement,
        Cobol85Parser.PerformStatementContext: process_perform_statement,
        Cobol85Parser.GobackStatementContext: process_goback_statement,
        Cobol85Parser.IfStatementContext: process_conditional_statement,
        Cobol85Parser.SubtractStatementContext: process_subtract,
        Cobol85Parser.SubtractCorrespondingStatementContext: process_subtract,
        Cobol85Parser.SubtractFromStatementContext: process_subtract,
        Cobol85Parser.SubtractFromGivingStatementContext: process_subtract,
        Cobol85Parser.ExecCicsStatementContext: visit_exec_cics_statement_context,
        Cobol85Parser.SortStatementContext: process_sort_statement,
        Cobol85Parser.AcceptStatementContext: process_io_statement,
        Cobol85Parser.DisplayStatementContext: process_io_statement,
        Cobol85Parser.ReadStatementContext: process_io_statement,
        Cobol85Parser.WriteStatementContext: process_io_statement,
        Cobol85Parser.OpenStatementContext: visit_resource_management_statement_context,
        Cobol85Parser.CloseStatementContext: visit_resource_management_statement_context,
        Cobol85Parser.InitializeStatementContext: visit_data_operation_statement_context,
        Cobol85Parser.InspectStatementContext: visit_data_operation_statement_context,
        Cobol85Parser.ExitStatementContext: visit_control_transfer_statement_context,
        Cobol85Parser.GoToStatementContext: visit_control_transfer_statement_context,
        Cobol85Parser.CallStatementContext: visit_invocation_statement_context,
    }

    # Optimization: Use type(ctx) directly if ctx is a statement context
    visitor = statement_map.get(type(ctx))
    if visitor:
        return visitor(ctx)

    # Fallback: check children if ctx is a wrapper node
    for child in context_info.get_children(ctx):
        visitor = statement_map.get(type(child))
        if visitor:
            return visitor(child)

    return None

def process_assigment_statement(assignment_statement):
    """
    Επεξεργάζεται μια assignment statement και επιστρέφει το αντίστοιχο AssignStatement.
    """
    if assignment_statement:
        assign_from = assignment_statement.moveToStatement().moveToSendingArea().getText()
        assigns = [identifier.getText() for identifier in assignment_statement.moveToStatement().identifier()]
        assign_to = ','.join(assigns)
        # assignment_statement.moveToStatement().moveToSendingArea().getText()
        return AssignStatement(AssignFrom=assign_from, AssignTo=assign_to)
    return None

def process_relationSignCondition(relation_sign_condition):
    """
    Επεξεργάζεται μια relationSignCondition και επιστρέφει το αντίστοιχο ConditionClause.
    """
    if relation_sign_condition.SIGN():
        sign = relation_sign_condition.SIGN().getText()
        return ConditionClause(sign=sign)
    return None

def process_relationCondition(relation_condition):
    """
    Επεξεργάζεται μια relationCondition και επιστρέφει το αντίστοιχο ConditionClause.
    """
    if relation_condition.relationArithmeticComparison():
        cond = relation_condition.relationArithmeticComparison().getText()
        s2 = relation_condition.relationCombinedComparison()
        s3 = relation_condition.relationSignCondition()
        return ConditionClause(condition=cond, combined=s2.getText() if s2 else "", sign=s3.getText() if s3 else "")
    return None

def process_simple_condition(simple_condition):
    """
    Επεξεργάζεται μια απλή συνθήκη και επιστρέφει το αντίστοιχο ConditionClause.
    """
    if simple_condition.relationCondition():
        relation = simple_condition.relationCondition()
        # cond = relation.relationArithmeticComparison().getText()
        # s2 = relation.relationCombinedComparison()
        operator = relation.relationArithmeticComparison().relationalOperator().getText()
        left = relation.relationArithmeticComparison().arithmeticExpression()[0].getText() if relation.relationArithmeticComparison().arithmeticExpression() else ""
        right = relation.relationArithmeticComparison().arithmeticExpression()[1].getText() if len(relation.relationArithmeticComparison().arithmeticExpression()) > 1 else ""
        # s3 = relation.relationSignCondition()
        return ConditionClause(Operator=operator, Left=left, Right=right)
    return None

def process_perform_until_condition(perform_until_condition):
    """
    Επεξεργάζεται μια PERFORM UNTIL condition και επιστρέφει το αντίστοιχο ConditionClause.
    """
    if perform_until_condition.combinableCondition():
        combinable = perform_until_condition.combinableCondition()
        if combinable.simpleCondition():
            simple = combinable.simpleCondition()
            return process_simple_condition(simple)
        elif combinable.andOrCondition():
            and_or = combinable.andOrCondition()
            # Process AND/OR conditions if needed
            return ConditionClause(condition=and_or.getText())
    return None

def process_perform_inline_statement(inline_statement):
    """
    Επεξεργάζεται ένα PERFORM inline statement και επιστρέφει τα statements που περιέχει.
    """
    statements = []
    for statement in inline_statement.statement():
        parsed_statement = parse_statement(statement)
        if parsed_statement:
            statements.append(parsed_statement)
            
    perform_type = inline_statement.performType()
    if perform_type:
        if perform_type.performTimes():
            method_name = inline_statement.performType().performTimes().getText()
        elif perform_type.performVarying():
            method_name = inline_statement.performType().performVarying().getText()
        elif perform_type.performUntil():
            cond = process_perform_until_condition(perform_type.performUntil().condition())
            return LoopStatement(condition=cond, Statements=statements)
            # if perform_type.performUntil().condition().combinableCondition():
            #     combinable = perform_type.performUntil().condition().combinableCondition()
            #     if combinable.simpleCondition():
            #         simple = combinable.simpleCondition()
            #         if simple.relationCondition():
            #             relation = simple.relationCondition()
            #             cond = relation.relationArithmeticComparison().getText()
            #             s2 = relation.relationCombinedComparison()
            #             s3 = relation.relationSignCondition()

                        # return LoopStatement(condition=cond, Statements=statments)
# ----------------------------------------------------
# Assignment Statements
# ----------------------------------------------------
def process_goback_statement(ctx):
    """ Επεξεργάζεται ένα GOBACK statement και επιστρέφει ένα CallStatement. """
    return InvocationStatement(methodName="GOBACK")

def process_perform_statement(ctx):
    if ctx.performInlineStatement():
        return process_perform_inline_statement(ctx.performInlineStatement())    
    elif ctx.performProcedureStatement():
        method_name = ctx.performProcedureStatement().procedureName()[0].getText()
        return InvocationStatement(methodName=f"PERFORM {method_name}")
    else:
        logger.warning("PERFORM statement does not contain a valid performInlineStatement or performProcedureStatement, cannot parse.")
    return None

def visit_perform_statement_context(ctx):
    """ Επεξεργάζεται ένα PERFORM statement και επιστρέφει ένα CallStatement. """
    if ctx.performInlineStatement():
        return process_perform_inline_statement(ctx.performInlineStatement())
        # inline_statement = ctx.performInlineStatement()
        # statements = inline_statement.statement()
        # statments = []
        # for statement in statements:
        #     parsed_statement = parse_statement(statement)
        #     if parsed_statement:
        #         statments.append(parsed_statement)
        # perform_type = inline_statement.performType()
        # if perform_type:
        #     if perform_type.performTimes():
        #         method_name = inline_statement.performType().performTimes().getText()
        #     elif perform_type.performVarying():
        #         method_name = inline_statement.performType().performVarying().getText()
        #     elif perform_type.performUntil():
        #         if perform_type.performUntil().condition().combinableCondition():
        #             combinable = perform_type.performUntil().condition().combinableCondition()
        #             if combinable.simpleCondition():
        #                 simple = combinable.simpleCondition()
        #                 if simple.relationCondition():
        #                     relation = simple.relationCondition()
        #                     cond = relation.relationArithmeticComparison().getText()
        #                     s2 = relation.relationCombinedComparison()
        #                     s3 = relation.relationSignCondition()

        #                     return LoopStatement(condition=cond, Statements=statments)
        #     elif perform_type.performUntil().condition().andOrCondition():
        #         and_or = perform_type.performUntil().condition().andOrCondition()
        #     elif perform_type.performUntil().condition().combinableCondition().simpleCondition():
        #         perform_type.performUntil().condition().combinableCondition().simpleCondition()
        #         method_name = inline_statement.performType().performUntil().getText()
        #     else:
        #         method_name = inline_statement.getText()
        #     return None

    elif ctx.performProcedureStatement():
        method_name = ctx.performProcedureStatement().procedureName()[0].getText()
        return InvocationStatement(methodName=f"PERFORM {method_name}")

        # ctx.performProcedureStatement().THRU()
        # ctx.performProcedureStatement().THROUGH()
        # pass
    else:
        logger.warning("PERFORM statement does not contain a valid performInlineStatement or performProcedureStatement, cannot parse.")
    return None
    # method_name = ctx.getChild(1).getText() if ctx.getChildCount() > 1 else ""

def visit_move_statement_context(ctx):
    """ Επεξεργάζεται ένα MOVE statement και επιστρέφει ένα AssignStatement. """
    assign_from = None
    assign_to = None
    
    moveToStatement = ctx.moveToStatement()
    if moveToStatement:
        assign_from = [item.getText() for item in moveToStatement.identifier()]
        assign_to = moveToStatement.moveToSendingArea().getText()
    else:
        logger.warning("MOVE statement does not contain a moveToStatement, cannot parse assignment.")
        
    # for child in context_info.get_children(ctx):
    #     if isinstance(child, Cobol85Parser.MoveToSendingAreaContext):
    #         assign_from = visit_move_to_sending_area_context(child)
    #     elif isinstance(child, Cobol85Parser.IdentifierContext):
    #         assign_to = visit_identifier_context(child)

    if assign_from and assign_to:
        return AssignStatement(methodName=f"Assign {assign_from} to {assign_to}", AssignFrom=assign_from, AssignTo=assign_to)
    return None


# def visit_move_to_sending_area_context(ctx):
#     """ Επιστρέφει το text του sending area. """
#     return ctx.getText()


# ----------------------------------------------------
# Invocation Statements (Call, Exec CICS)
# ----------------------------------------------------

def visit_invocation_statement_context(ctx):
    """ Επεξεργάζεται ένα CALL statement. """
    method_name = ctx.getChild(1).getText() if ctx.getChildCount() > 1 else ""
    return CallStatement(methodName=f"CALL {method_name}")


def visit_exec_cics_statement_context(ctx):
    """ Επεξεργάζεται ένα EXEC CICS statement. """
    call_statement = CallCicsStatement(methodName="EXEC CICS")
    for child in context_info.get_children(ctx):
        if isinstance(child, Cobol85Parser.ExecCicsCommandContext):
            command_name = child.getChild(0).getText()
            call_statement.addStatement(Statement(methodName=command_name))
    return call_statement


def process_sort_statement(ctx):
    """ Επεξεργάζεται ένα SORT statement. """
    operation = ctx.SORT().getText()
    arguments = [ctx.fileName().getText()]
    parameters = {}
    extra = {}
    sort_statement = DataOperationStatement(methodName="SORT")

    onKeyClause = ctx.sortOnKeyClause()
    if onKeyClause:
        for child in onKeyClause:
            if isinstance(child, Cobol85Parser.SortOnKeyClauseContext):                
                key = child.ASCENDING().getText() if child.ASCENDING() else child.DESCENDING().getText()
                value = [val.getText() for val in child.qualifiedDataName()]
                parameters[key] = value
                

    inputProcedure = ctx.sortInputProcedurePhrase()
    if inputProcedure:
       thru = inputProcedure.sortInputThrough()
       if thru:
            from_procedure = inputProcedure.procedureName().getText()
            to_procedure = thru.procedureName().getText()
            throughValue = inputProcedure.PROCEDURE().getText() if inputProcedure.PROCEDURE() else  ""

            extra[f'INPUT {throughValue}'] = [from_procedure, to_procedure]

    outputProcedure = ctx.sortOutputProcedurePhrase()
    if outputProcedure:
       thru = outputProcedure.sortOutputThrough()
       if thru:
            from_procedure = outputProcedure.procedureName().getText()
            to_procedure = thru.procedureName().getText()
            throughValue = outputProcedure.PROCEDURE().getText() if outputProcedure.PROCEDURE() else  ""

            extra[f'OUTPUT {throughValue}'] = [from_procedure, to_procedure]

    return DataOperationStatement(
            operation=operation,
            arguments=arguments,
            parameters=parameters,
            extra=extra
        )
    
def process_conditions(ctx):
    if ctx.combinableCondition():
        combinible = ctx.combinableCondition()
        if combinible.simpleCondition():
           return process_simple_condition(combinible.simpleCondition())
        elif combinible.andOrCondition():
            and_or = combinible.andOrCondition()
            # Process AND/OR conditions if needed
            return ConditionClause(condition=and_or.getText())    
        else:
            logger.warning("Condition Handling not implemented!") 
                      
# ----------------------------------------------------
# Calculation Statements
# ----------------------------------------------------           
def process_subtract(ctx):           

    if ctx.subtractFromGivingStatement():
        from_giving = ctx.subtractFromGivingStatement()
        subtract = ','.join(x.getText() for x in from_giving.subtractGiving())
        subFrom = from_giving.subtractMinuendGiving().getText()
        giving = ','.join(x.getText() for x in from_giving.subtractSubtrahend())
        return AssignStatement(AssignFrom=f'{subFrom} - {subtract}', AssignTo=giving)
    elif ctx.subtractCorrespondingStatement():
        from_corresponding = ctx.subtractCorrespondingStatement()  
        logger.warning(f"{ctx.subtractCorrespondingStatement().getText()} not processed.")
    elif ctx.subtractFromStatement():
        from_statement = ctx.subtractFromStatement()
        logger.warning(f"{ctx.subtractFromStatement().getText()} not processed.")
    else:
        logger.warning(f"{ctx.getText()} not processed.")
    
# ----------------------------------------------------
# Conditional Statements
# ----------------------------------------------------


def process_conditional_statement(ctx):
    cond = process_conditions(ctx.condition())
    condition = ConditionalStatement(methodName="IF", conditionClauses=cond)
    if ctx.ifThen() and ctx.ifThen().statement():
        then_clause = ctx.ifThen().statement()
        if then_clause:
            for st in then_clause:
                try:
                    statement_to_add = parse_statement(st)
                    condition.addTrueStatement(parse_statement(st))
                except Exception as ex:
                    logger.info(ex)
    if ctx.ifElse() and ctx.ifElse().statement():
        else_clause = ctx.ifElse().statement()
        if else_clause:
            for st in else_clause:
                condition.addFalseStatement(parse_statement(st))
    return condition

def visit_if_statement_context(ctx):
    """ Επεξεργάζεται ένα IF statement. """
    conditional_statement = ConditionalStatement(methodName="IF")
    for child in context_info.get_children(ctx):
        if isinstance(child, Cobol85Parser.IfThenContext):
            visit_if_then_context(child, conditional_statement)
        elif isinstance(child, Cobol85Parser.IfElseContext):
            visit_if_else_context(child, conditional_statement)

    return conditional_statement


def visit_if_then_context(ctx, conditional_statement):
    """ Επεξεργάζεται το THEN μέρος του IF. """
    for child in context_info.get_children(ctx):
        if isinstance(child, Cobol85Parser.StatementContext):
            statement = parse_statement(child)
            if statement:
                conditional_statement.addTrueStatement(statement)


def visit_if_else_context(ctx, conditional_statement):
    """ Επεξεργάζεται το ELSE μέρος του IF. """
    for child in context_info.get_children(ctx):
        if isinstance(child, Cobol85Parser.StatementContext):
            statement = parse_statement(child)
            if statement:
                conditional_statement.addFalseStatement(statement)


# ----------------------------------------------------
# Control-Transfer Statements
# ----------------------------------------------------

def visit_control_transfer_statement_context(ctx):
    """ Επεξεργάζεται τα GO TO, EXIT, GOBACK statements. """
    command_name = ctx.getChild(0).getText().upper()
    return CallStatement(methodName=command_name)


# ----------------------------------------------------
# I/O Statements
# ----------------------------------------------------

def process_io_statement(ctx):
    """ Επεξεργάζεται τα I/O statements (ACCEPT, DISPLAY, READ, WRITE). """
    operation = ctx.getChild(0).getText().upper()
    target = ctx.getChild(1).getText() if ctx.getChildCount() > 1 else ""
    return IOStatement(io_keyword=operation, target=target)


# ----------------------------------------------------
# Data-Operation Statements
# ----------------------------------------------------

def visit_data_operation_statement_context(ctx):
    """ Επεξεργάζεται τα Data-Operation statements (INITIALIZE, INSPECT). """
    operation = ctx.getChild(0).getText().upper()
    target = ctx.getChild(1).getText() if ctx.getChildCount() > 1 else ""
    return DataOperationStatement(methodName=f"{operation} {target}")


# ----------------------------------------------------
# Resource-Management Statements
# ----------------------------------------------------

def visit_resource_management_statement_context(ctx):
    """ Επεξεργάζεται τα Resource-Management statements (OPEN, CLOSE). """
    operation = ctx.getChild(0).getText().upper()
    target = ctx.getChild(1).getText() if ctx.getChildCount() > 1 else ""
    return Statement(methodName=f"{operation} {target}")


# ----------------------------------------------------
# Identifier Helper
# ----------------------------------------------------

def visit_identifier_context(ctx):
    """ Επιστρέφει το text του identifier. """
    return ctx.getText()
