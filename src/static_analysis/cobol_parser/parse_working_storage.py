import json
from grammars.Cobol85Parser import Cobol85Parser
import context_info
from models.StaticAnalysis import StaticAnalysis  # Functional context helper
from logger import logger
import re

def parse_working_storage_section(ctx, static_analysis):
    """
    Αναλύει το WORKING-STORAGE SECTION και εξάγει τις δηλώσεις μεταβλητών.
    Υποστηρίζει `REDEFINES`, `OCCURS`, και `88 LEVEL` conditions.
    :param ctx: Το parse tree context του WORKING-STORAGE SECTION.
    :return: Μια λίστα με μεταβλητές που δηλώθηκαν.
    """
    # workingStorageTree = find_working_storage_section(ctx)
    variables = []
    parent_entry = None  # Για να διατηρούμε parent για τα 88 LEVEL conditions

    for child in context_info.get_children(ctx):
        if isinstance(child, Cobol85Parser.DataDescriptionEntryContext):
            var_entry = visit_data_description_entry(child)

            if var_entry:
                # Αν είναι condition (88 LEVEL), το προσθέτουμε στο parent
                if var_entry["Level"] == "88" and parent_entry:
                    parent_entry["CONDITIONS"].append(var_entry)
                else:
                    # Αν είναι κανονική εγγραφή, την αποθηκεύουμε
                    variables.append(var_entry)
                    parent_entry = var_entry  # Ορίζουμε το parent για τις επόμενες conditions
    logger.info(variables)
    static_analysis.DataStructures = variables
    return static_analysis

def visit_data_description_entry(ctx):
    """
    Επεξεργάζεται κάθε εγγραφή περιγραφής δεδομένων (μεταβλητή).
    :param ctx: Το parse tree context του DataDescriptionEntry.
    :return: Ένα dictionary με τις πληροφορίες της μεταβλητής.
    """
    var_entry = {
        "Level": None,
        "Name": None,
        "PIC": None,
        "VALUE": None,
        "OCCURS": None,
        "REDEFINES": None,
        "CONDITIONS": []  # Προσθήκη για 88 LEVEL conditions
    }

    for child in context_info.get_children(ctx):
        var_entry["Level"] = context_info.get_child_concatenated_text(child, 0)
        
        if isinstance(child, Cobol85Parser.DataDescriptionEntryFormat1Context):
            for sub_child in context_info.get_children(child):
                if isinstance(sub_child, Cobol85Parser.DataNameContext):
                    var_entry["Name"] = sub_child.getText()
                elif isinstance(sub_child, Cobol85Parser.DataPictureClauseContext):
                    var_entry["PIC"] = get_value(sub_child)
                elif isinstance(sub_child, Cobol85Parser.DataValueClauseContext):
                    var_entry["VALUE"] = get_value(sub_child)
                elif isinstance(sub_child, Cobol85Parser.DataRedefinesClauseContext):
                    var_entry["REDEFINES"] = get_value(sub_child)
                elif isinstance(sub_child, Cobol85Parser.DataOccursClauseContext):
                    var_entry["OCCURS"] = get_value(sub_child)

        elif isinstance(child, Cobol85Parser.DataDescriptionEntryFormat3Context):
            condition_entry = visit_condition_clause(child)
            return condition_entry  # Επιστρέφουμε το condition ώστε να προστεθεί στο parent


    # Αν η μεταβλητή είναι `FILLER`, αποθηκεύουμε "FILLER" αντί για None
    if var_entry["Name"] is None:
        var_entry["Name"] = "FILLER"

    return var_entry

def get_value(ctx) -> str:
    """ Εξάγει την `PIC` δήλωση. """
    if ctx.getChildCount() > 1:
        return ctx.getChild(1).getText().strip()
    else:
        return ""

def visit_picture_clause(ctx) -> str:
    """ Εξάγει την `PIC` δήλωση. """
    if ctx.getChildCount() > 1:
        return ctx.getChild(1).getText().strip()
    else:
        return ""

def visit_value_clause(ctx) -> str:
    """ Εξάγει την `VALUE` δήλωση. """
    if ctx.getChildCount() > 1:
        return ctx.getChild(1).getText().strip()
    else:
        return ""

def visit_occurs_clause(ctx) -> str:
    """ Εξάγει την `OCCURS` δήλωση. """
    if ctx.getChildCount() > 1:
        return ctx.getChild(1).getText().strip()
    else:
        return ""

def visit_redefines_clause(ctx) -> str:
    """ Εξάγει την `REDEFINES` δήλωση. """
    if ctx.getChildCount() > 1:
        return ctx.getChild(1).getText().strip()
    else:
        return ""

def visit_condition_clause(ctx):
    """ 
    Επεξεργάζεται τα 88 LEVEL condition names και τις τιμές τους.
    :param ctx: Το parse tree context του 88 LEVEL condition.
    :return: Ένα dictionary με το condition και το VALUE του.
    """
    condition = {"Level": "88", "Name": None, "VALUE": None}

    for child in context_info.get_children(ctx):
        if isinstance(child, Cobol85Parser.ConditionNameContext):
            condition["Name"] = child.getText()
        elif isinstance(child, Cobol85Parser.DataValueClauseContext):
            condition["VALUE"] = visit_value_clause(child)

    return condition

def get_variables_json(variables) -> str:
    """ Επιστρέφει τις μεταβλητές σε μορφή JSON. """
    return json.dumps(variables, indent=4)

def get_variables_raw(variables) -> str:
    """ Επιστρέφει τις μεταβλητές σε μορφή JSON. """
    return variables    

def variables_to_json(variables) -> str:
    """ Επιστρέφει τις μεταβλητές σε μορφή JSON. """
    return convert_cobol_to_json(variables)

def cobol_pic_to_json_type(pic):
    """
    Μετατρέπει το COBOL PIC format σε JSON-compatible type.
    """
    if pic is None:
        return "object"  # Αν δεν υπάρχει PIC, πιθανώς είναι struct container
    
    # Χειρισμός string (PIC X(n))
    match = re.match(r'X\((\d+)\)', pic)
    if match:
        return {"type": "string", "length": int(match.group(1))}
    
    # Χειρισμός ακεραίων (PIC 9(n))
    match = re.match(r'S?9\((\d+)\)', pic)
    if match:
        return {"type": "integer", "length": int(match.group(1))}
    
    # Χειρισμός δεκαδικών (PIC S9(n)V9(m))
    match = re.match(r'S?9\((\d+)\)V9\((\d+)\)', pic)
    if match:
        return {"type": "decimal", "integer_part": int(match.group(1)), "decimal_part": int(match.group(2))}
    
    return {"type": "unknown"}

def convert_cobol_to_json(variables):
    """
    Μετατρέπει COBOL variables σε δομή JSON με nested records.
    """
    json_structure = []
    record_stack = []  # Stack για nested structures

    for var in variables:
        json_entry = {
            "name": var["Name"] if var["Name"] != "FILLER" else f"unused_{id(var)}",
            "level": int(var["Level"]),
            "type": cobol_pic_to_json_type(var["PIC"]),
            "fields": []
        }

        # Αν υπάρχει OCCURS, τότε είναι array
        if var["OCCURS"]:
            json_entry["occurs"] = int(var["OCCURS"])

        # Αν υπάρχει REDEFINES, το αποθηκεύουμε
        if var["REDEFINES"]:
            json_entry["redefines"] = var["REDEFINES"]

        # **Διαχείριση δομής records/fields**
        while record_stack and record_stack[-1]["level"] >= json_entry["level"]:
            record_stack.pop()  # Αφαιρούμε ανώτερα επίπεδα πριν προσθέσουμε το νέο

        if json_entry["level"] == 1:
            json_structure.append(json_entry)
            record_stack = [json_entry]  # Reset το stack σε νέο record
        else:
            if record_stack:
                record_stack[-1]["fields"].append(json_entry)
            record_stack.append(json_entry)  # Προσθήκη στο stack

    return json.dumps(json_structure, indent=4)


# def find_working_storage_section(ctx):
#     """
#     Αναζητά αναδρομικά το WorkingStorageSection σε ένα υποδέντρο του parse tree.
#     """
#     if ctx is None:
#         return None

#     # Αν ο κόμβος είναι ήδη WorkingStorageSection, επιστρέφουμε
#     if isinstance(ctx, Cobol85Parser.WorkingStorageSectionContext):
#         return ctx

#     # Αναζητάμε αναδρομικά σε όλους τους υποκόμβους
#     for i in range(ctx.getChildCount()):
#         result = find_working_storage_section(ctx.getChild(i))
#         if result is not None:
#             return result  # Βρήκαμε το WorkingStorageSection, σταματάμε την αναζήτηση

#     return None  # Αν δεν βρέθηκε τίποτα