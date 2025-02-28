import re
import json

def cobol_pic_to_c_type(pic):
    """
    Μετατρέπει το COBOL PIC format σε C τύπο δεδομένων.
    """
    if pic is None:
        return "void"  # Αν δεν υπάρχει PIC, πιθανώς είναι struct container
    
    # Χειρισμός string (PIC X(n))
    match = re.match(r'X\((\d+)\)', pic)
    if match:
        length = int(match.group(1))
        return f"char[{length + 1}]"  # Null-terminated string

    # Χειρισμός ακεραίων (PIC 9(n))
    match = re.match(r'S?9\((\d+)\)', pic)
    if match:
        length = int(match.group(1))
        if length <= 4:
            return "short"
        elif length <= 9:
            return "int"
        else:
            return "long long"

    # Χειρισμός δεκαδικών (PIC S9(n)V9(m))
    match = re.match(r'S?9\((\d+)\)V9\((\d+)\)', pic)
    if match:
        return "double"  # Δεκαδικοί αριθμοί αποθηκεύονται ως double
    
    return "void"  # Default fallback

def convert_cobol_to_c(variables):
    """
    Μετατρέπει λίστα COBOL variables σε C struct.
    """
    struct_lines = ["typedef struct {"]
    
    for var in variables:
        c_type = cobol_pic_to_c_type(var["PIC"])
        var_name = var["Name"] if var["Name"] != "FILLER" else f"unused_{id(var)}"

        # Αν υπάρχει OCCURS, τότε είναι array
        if var["OCCURS"]:
            struct_lines.append(f"    {c_type} {var_name}[{var['OCCURS']}];")
        else:
            struct_lines.append(f"    {c_type} {var_name};")

    struct_lines.append("} CobolStruct;")
    
    return "\n".join(struct_lines)

# Παράδειγμα χρήσης:
# variables = [
#     {"Level": "01", "Name": "DOGEMSG", "PIC": None, "OCCURS": None},
#     {"Level": "05", "Name": "DOGEID", "PIC": "X(10)", "OCCURS": None},
#     {"Level": "05", "Name": "AMOUNT", "PIC": "S9(4)V9(2)", "OCCURS": None},
#     {"Level": "05", "Name": "TRANSACTION", "PIC": "9(9)", "OCCURS": None},
#     {"Level": "05", "Name": "DETAILS", "PIC": "X(50)", "OCCURS": "5"},
# ]

variables = [{'Level': '77', 'Name': 'RC', 'PIC': 'S9(4)', 'VALUE': '+0', 'OCCURS': None, 'REDEFINES': None, 'CONDITIONS': []}, {'Level': '77', 'Name': 'SYSOUT-TOKEN', 'PIC': 'X(8)', 'VALUE': 'SPACES', 'OCCURS': None, 'REDEFINES': None, 'CONDITIONS': []}, {'Level': '01', 'Name': 'DOGEMSG', 'PIC': None, 'VALUE': None, 'OCCURS': None, 'REDEFINES': None, 'CONDITIONS': []}, {'Level': '05', 'Name': 'DOGEID', 'PIC': 'X(10)B', 'VALUE': "'DOGECICS99'", 'OCCURS': None, 'REDEFINES': None, 'CONDITIONS': []}, {'Level': '05', 'Name': 'ADDRSS', 'PIC': 'X(34)B', 'VALUE': None, 'OCCURS': None, 'REDEFINES': None, 'CONDITIONS': []}, {'Level': '05', 'Name': 'AMOUNT', 'PIC': 'Z(02),Z(03),Z(02)9.9(8)', 'VALUE': None, 'OCCURS': None, 'REDEFINES': None, 'CONDITIONS': []}, {'Level': '01', 'Name': 'DOGEMSG-LEN', 'PIC': '99', 'VALUE': '61', 'OCCURS': None, 'REDEFINES': None, 'CONDITIONS': []}, {'Level': '01', 'Name': 'TRANSACTION', 'PIC': None, 'VALUE': None, 'OCCURS': None, 'REDEFINES': None, 'CONDITIONS': []}, {'Level': '05', 'Name': 'TDATE', 'PIC': 'X(10)', 'VALUE': None, 'OCCURS': None, 'REDEFINES': None, 'CONDITIONS': []}, {'Level': '05', 'Name': 'NUM-DATE', 'PIC': '9(10)', 'VALUE': None, 'OCCURS': None, 'REDEFINES': 'TDATE', 'CONDITIONS': []}, {'Level': '05', 'Name': 'FILLER', 'PIC': 'X', 'VALUE': 'SPACES', 'OCCURS': None, 'REDEFINES': None, 'CONDITIONS': []}, {'Level': '05', 'Name': 'TADDRSS', 'PIC': 'X(34)', 'VALUE': None, 'OCCURS': None, 'REDEFINES': None, 'CONDITIONS': []}, {'Level': '05', 'Name': 'FILLER', 'PIC': 'X', 'VALUE': 'SPACES', 'OCCURS': None, 'REDEFINES': None, 'CONDITIONS': []}, {'Level': '05', 'Name': 'TLABEL', 'PIC': 'X(10)', 'VALUE': None, 'OCCURS': None, 'REDEFINES': None, 'CONDITIONS': []}, {'Level': '05', 'Name': 'FILLER', 'PIC': 'X', 'VALUE': 'SPACES', 'OCCURS': None, 'REDEFINES': None, 'CONDITIONS': []}, {'Level': '05', 'Name': 'TAMOUNT', 'PIC': None, 'VALUE': None, 'OCCURS': None, 'REDEFINES': None, 'CONDITIONS': []}, {'Level': '10', 'Name': 'TAMT-SIGN', 'PIC': 'X', 'VALUE': None, 'OCCURS': None, 'REDEFINES': None, 'CONDITIONS': [{'Level': '88', 'Name': 'TAMT-SIGN-POSITIVE', 'VALUE': "'+'"}, {'Level': '88', 'Name': 'TAMT-SIGN-NEGATIVE', 'VALUE': "'-'"}]}, {'Level': '10', 'Name': 'TAMT-INTEGER-PART', 'PIC': 'X(8)', 'VALUE': None, 'OCCURS': None, 'REDEFINES': None, 'CONDITIONS': []}, {'Level': '10', 'Name': 'TAMT-DEC-POINT', 'PIC': 'X', 'VALUE': None, 'OCCURS': None, 'REDEFINES': None, 'CONDITIONS': []}, {'Level': '10', 'Name': 'TAMT-DECIMAL-PART', 'PIC': 'X(8)', 'VALUE': None, 'OCCURS': None, 'REDEFINES': None, 'CONDITIONS': []}, {'Level': '01', 'Name': 'THE-AMOUNT', 'PIC': 'S9(8)V9(8)', 'VALUE': None, 'OCCURS': None, 'REDEFINES': None, 'CONDITIONS': []}, {'Level': '01', 'Name': 'FILLER', 'PIC': None, 'VALUE': None, 'OCCURS': None, 'REDEFINES': 'THE-AMOUNT', 'CONDITIONS': []}, {'Level': '05', 'Name': 'THE-AMOUNT-INTEGER', 'PIC': 'X(8)', 'VALUE': None, 'OCCURS': None, 'REDEFINES': None, 'CONDITIONS': []}, {'Level': '05', 'Name': 'THE-AMOUNT-DECIMAL', 'PIC': 'S9(8)', 'VALUE': None, 'OCCURS': None, 'REDEFINES': None, 'CONDITIONS': []}, {'Level': '01', 'Name': 'AVAILABLE-AMOUNT', 'PIC': 'S9(8)V9(8)', 'VALUE': None, 'OCCURS': None, 'REDEFINES': None, 'CONDITIONS': []}, {'Level': '01', 'Name': 'RECENT-COLOR', 'PIC': 'X', 'VALUE': None, 'OCCURS': None, 'REDEFINES': None, 'CONDITIONS': []}, {'Level': '01', 'Name': 'DISPLAY-TRAN', 'PIC': None, 'VALUE': None, 'OCCURS': None, 'REDEFINES': None, 'CONDITIONS': []}, {'Level': '05', 'Name': 'DDATE', 'PIC': 'X(10)', 'VALUE': None, 'OCCURS': None, 'REDEFINES': None, 'CONDITIONS': []}, {'Level': '05', 'Name': 'FILLER', 'PIC': 'X', 'VALUE': 'SPACES', 'OCCURS': None, 'REDEFINES': None, 'CONDITIONS': []}, {'Level': '05', 'Name': 'DLABEL', 'PIC': 'X(10)', 'VALUE': None, 'OCCURS': None, 'REDEFINES': None, 'CONDITIONS': []}, {'Level': '05', 'Name': 'FILLER', 'PIC': 'X', 'VALUE': 'SPACES', 'OCCURS': None, 'REDEFINES': None, 'CONDITIONS': []}, {'Level': '05', 'Name': 'DSIGN', 'PIC': 'X', 'VALUE': None, 'OCCURS': None, 'REDEFINES': None, 'CONDITIONS': []}, {'Level': '05', 'Name': 'DAMOUNT', 'PIC': 'Z(02),Z(03),Z(02)9.9(8)', 'VALUE': None, 'OCCURS': None, 'REDEFINES': None, 'CONDITIONS': []}, {'Level': '05', 'Name': 'FILLER', 'PIC': 'X', 'VALUE': 'SPACES', 'OCCURS': None, 'REDEFINES': None, 'CONDITIONS': []}, {'Level': '05', 'Name': 'DTYPE', 'PIC': 'XXXX', 'VALUE': "'DOGE'", 'OCCURS': None, 'REDEFINES': None, 'CONDITIONS': []}, {'Level': '01', 'Name': 'TEMP-DATE', 'PIC': '9(15)', 'VALUE': None, 'OCCURS': None, 'REDEFINES': None, 'CONDITIONS': []}, {'Level': '01', 'Name': 'DOGEMSG-LEN', 'PIC': '99', 'VALUE': '61', 'OCCURS': None, 'REDEFINES': None, 'CONDITIONS': []}, {'Level': '01', 'Name': 'START-RECORD-ID', 'PIC': '9(10)', 'VALUE': '9999999999', 'OCCURS': None, 'REDEFINES': None, 'CONDITIONS': []}, {'Level': '01', 'Name': 'SINCE-EPOCH', 'PIC': 'S9(15)', 'VALUE': '+2208988800000', 'OCCURS': None, 'REDEFINES': None, 'CONDITIONS': []}, {'Level': '01', 'Name': 'RESPONSE-CODE', 'PIC': 'S9(4)', 'VALUE': None, 'OCCURS': None, 'REDEFINES': None, 'CONDITIONS': []}, {'Level': '01', 'Name': 'RESPONSE-CODE2', 'PIC': 'S9(4)', 'VALUE': None, 'OCCURS': None, 'REDEFINES': None, 'CONDITIONS': []}, {'Level': '01', 'Name': 'DOGECOMMS-AREA', 'PIC': None, 'VALUE': None, 'OCCURS': None, 'REDEFINES': None, 'CONDITIONS': []}, {'Level': '05', 'Name': 'DOGE-FLAG', 'PIC': 'X', 'VALUE': None, 'OCCURS': None, 'REDEFINES': None, 'CONDITIONS': [{'Level': '88', 'Name': 'SUCH-DOGE', 'VALUE': "'D'"}, {'Level': '88', 'Name': 'WOW-MENU', 'VALUE': "'W'"}, {'Level': '88', 'Name': 'SUCH-SEND', 'VALUE': "'S'"}, {'Level': '88', 'Name': 'SUCH-HISTORY', 'VALUE': "'T'"}]}, {'Level': '05', 'Name': 'FILLER', 'PIC': 'X(9)', 'VALUE': None, 'OCCURS': None, 'REDEFINES': None, 'CONDITIONS': []}, {'Level': '01', 'Name': 'WTO-MESSAGE', 'PIC': 'X(38)', 'VALUE': 'SPACES', 'OCCURS': None, 'REDEFINES': None, 'CONDITIONS': []}]
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


print(convert_cobol_to_json(variables))
