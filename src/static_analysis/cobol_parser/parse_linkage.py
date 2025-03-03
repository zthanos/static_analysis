import json
from grammars.Cobol85Parser import Cobol85Parser
import context_info
from models.StaticAnalysis import StaticAnalysis
from logger import logger

def parse_linkage_section(ctx, static_analysis):
    """
    Αναλύει το LINKAGE SECTION και εξάγει τις δηλώσεις μεταβλητών.
    Υποστηρίζει `REDEFINES`, `OCCURS`, `88 LEVEL` conditions.
    :param ctx: Το parse tree context του LINKAGE SECTION.
    :return: Μια λίστα με μεταβλητές που δηλώθηκαν.
    """
    variables = []
    parent_entry = None
    if ctx is None: 
        logger.info(f"Program {static_analysis.ProgramId} doesn not contain Linkage Section")
        return static_analysis
    for child in context_info.get_children(ctx):
        if isinstance(child, Cobol85Parser.DataDescriptionEntryContext):
            var_entry = visit_data_description_entry(child)

            if var_entry:
                if var_entry["Level"] == "88" and parent_entry:
                    parent_entry["CONDITIONS"].append(var_entry)
                else:
                    variables.append(var_entry)
                    parent_entry = var_entry

    logger.info(variables)
    static_analysis.LinkageStructures = variables
    return static_analysis

def visit_data_description_entry(ctx):
    """
    Επεξεργάζεται κάθε εγγραφή στο LINKAGE SECTION.
    :param ctx: Το parse tree context του DataDescriptionEntry.
    :return: Dictionary με τις πληροφορίες της μεταβλητής.
    """
    var_entry = {
        "Level": None,
        "Name": None,
        "PIC": None,
        "VALUE": None,
        "OCCURS": None,
        "REDEFINES": None,
        "CONDITIONS": []
    }

    for child in context_info.get_children(ctx):
        var_entry["Level"] = context_info.get_child_concatenated_text(child, 0)

        if isinstance(child, Cobol85Parser.DataDescriptionEntryFormat1Context):
            for sub_child in context_info.get_children(child):
                if isinstance(sub_child, Cobol85Parser.DataNameContext):
                    var_entry["Name"] = sub_child.getText()
                elif isinstance(sub_child, Cobol85Parser.DataPictureClauseContext):
                    var_entry["PIC"] = visit_picture_clause(sub_child)
                elif isinstance(sub_child, Cobol85Parser.DataValueClauseContext):
                    var_entry["VALUE"] = visit_value_clause(sub_child)
                elif isinstance(sub_child, Cobol85Parser.DataRedefinesClauseContext):
                    var_entry["REDEFINES"] = visit_redefines_clause(sub_child)
                elif isinstance(sub_child, Cobol85Parser.DataOccursClauseContext):
                    var_entry["OCCURS"] = visit_occurs_clause(sub_child)
        elif isinstance(child, Cobol85Parser.DataDescriptionEntryFormat3Context):
            condition_entry = visit_condition_clause(child)
            if condition_entry:
                return condition_entry

    if var_entry["Name"] is None:
        var_entry["Name"] = "FILLER"

    return var_entry

def visit_picture_clause(ctx):
    return ctx.getText().replace("PIC", "").strip()

def visit_value_clause(ctx):
    return ctx.getText().replace("VALUE", "").strip()

def visit_occurs_clause(ctx):
    return ctx.getText().replace("OCCURS", "").strip()

def visit_redefines_clause(ctx):
    return ctx.getText().replace("REDEFINES", "").strip()

def visit_condition_clause(ctx):
    """ 
    Επεξεργάζεται τα 88 LEVEL condition names και τις τιμές τους.
    :param ctx: Το parse tree context του 88 LEVEL condition.
    :return: Dictionary με το condition και το VALUE του.
    """
    condition = {"Level": "88", "Name": None, "VALUE": None}
    
    for child in context_info.get_children(ctx):
        if isinstance(child, Cobol85Parser.ConditionNameContext):
            condition["Name"] = child.getText()
        elif isinstance(child, Cobol85Parser.DataValueClauseContext):
            condition["VALUE"] = visit_value_clause(child)

    return condition

def get_linkage_json(variables):
    """ Επιστρέφει τις μεταβλητές του LINKAGE SECTION σε JSON. """
    return json.dumps(variables, indent=4)
