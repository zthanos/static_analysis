import json
import re
from logger import logger
from grammars.Cobol85Visitor import Cobol85Visitor
from grammars.Cobol85Parser import Cobol85Parser

class CobolVisitor(Cobol85Visitor):
    def __init__(self):
        # Δομές για τα entry points και τις πληροφορίες τους
        self.entry_points = []           # Ονόματα entry points
        self.entry_inputs = {}           # { entry: [input1, input2, ...] }
        self.entry_outputs = {}          # { entry: [output1, output2, ...] }
        self.calls = {}                  # { entry: [call_obj, ...] } (προαιρετικά)
        # Δομή για το Flow (πλέον θα το εμφανίζουμε ως "Calls" στο τελικό JSON)
        self.flow_calls = {}             # { entry: [call_obj, ...] }
        self.current_entry = None
        # Ιδιότητες από το Identification Division
        self.identification_properties = {}

    def clean_text(self, text):
        """Αφαιρεί περιττούς χαρακτήρες."""
        return text.replace("(", "").replace(")", "").replace("'", "").strip()

    def get_recursive_text(self, ctx):
        """Αναδρομικά συλλέγει το κείμενο από τον κόμβο και όλους τους απογόνους του."""
        if ctx.getChildCount() == 0:
            return ctx.getText()
        else:
            parts = []
            for i in range(ctx.getChildCount()):
                parts.append(self.get_recursive_text(ctx.getChild(i)))
                logger.info("=======================>", ctx.getChild(i).getText())
            return " ".join(parts)

    def _init_entry(self, entry):
        """Αρχικοποιεί τις δομές για ένα entry point."""
        if entry not in self.entry_points:
            self.entry_points.append(entry)
        self.entry_inputs.setdefault(entry, [])
        self.entry_outputs.setdefault(entry, [])
        self.calls.setdefault(entry, [])
        self.flow_calls.setdefault(entry, [])

    # --- Εξαγωγή πληροφοριών από το Identification Division ---
    def visitIdentificationDivision(self, ctx):
        """
        Συλλέγει τις πληροφορίες από το Identification Division.  
        Αν το κείμενο είναι κάτι σαν:
        
            IDENTIFICATION DIVISION .
                PROGRAM-ID .    DOGECOIN .
                AUTHOR. SOLDIER OF FORTRAN.
                INSTALLATION. DOGE BANK.
                DATE-WRITTEN. 08/30/20.
                SECURITY. CONFIDENTIAL.
        
        τότε εξάγει τα πεδία: ProgramID, AUTHOR, INSTALLATION, DATE-WRITTEN, SECURITY.
        """
        full_text = self.get_recursive_text(ctx)
        lines = full_text.split("\n")
        properties = {}
        # Χρησιμοποιούμε regex για να εξάγουμε τα πεδία που μας ενδιαφέρουν
        pattern = re.compile(r"^(PROGRAM-ID|AUTHOR|INSTALLATION|DATE-WRITTEN|SECURITY)\s*\.\s*(.*?)\s*\.", re.IGNORECASE)
        for line in lines:
            line = line.strip()
            match = pattern.search(line)
            if match:
                key = match.group(1).strip()
                value = match.group(2).strip()
                if key.upper() == "PROGRAM-ID":
                    properties["ProgramID"] = value
                elif key.upper() == "AUTHOR":
                    properties["AUTHOR"] = value
                elif key.upper() == "INSTALLATION":
                    properties["INSTALLATION"] = value
                elif key.upper() == "DATE-WRITTEN":
                    properties["DATE-WRITTEN"] = value
                elif key.upper() == "SECURITY":
                    properties["SECURITY"] = value
        self.identification_properties = properties
        logger.info(f"[IDENTIFICATION] {self.identification_properties}")
        return self.visitChildren(ctx)

    def visitProgramIdParagraph(self, ctx):
        program_name_ctx = ctx.getTypedRuleContext(Cobol85Parser.ProgramNameContext, 0)
        if program_name_ctx is not None:
            program_name = self.clean_text(program_name_ctx.getText())
            self.current_entry = program_name
            self._init_entry(program_name)
            # logger.info(f"[ENTRY POINT] Program Name: {program_name}")
        else:
            logger.info("[ERROR] Program name not found in ProgramIdParagraph!")
        return self.visitChildren(ctx)

    def visitParagraph(self, ctx):
        if ctx.getChildCount() > 0:
            paragraph_name = ctx.getChild(0).getText().strip()
            self.current_entry = paragraph_name
            self._init_entry(paragraph_name)
            logger.info(f"[ENTRY] {paragraph_name}")
        return self.visitChildren(ctx)

    # --- Καταγραφή των κλήσεων / statements στο Flow (Calls) ---
    def visitCallStatement(self, ctx):
        children_texts = [child.getText().upper() for child in ctx.getChildren()]
        if "CALL" in children_texts or "XCTL" in children_texts:
            idx = (children_texts.index("CALL") + 1) if "CALL" in children_texts else (children_texts.index("XCTL") + 1)
            if idx < len(children_texts):
                call_text = self.clean_text(self.get_recursive_text(ctx.getChild(idx)))
                call_obj = { "description": call_text, "type": "execution", "comments": "" }
                if self.current_entry:
                    self.calls[self.current_entry].append(call_obj)
                    self.flow_calls[self.current_entry].append(call_obj)
                    # logger.info(f"[FLOW] {self.current_entry} → {call_obj}")
        return self.visitChildren(ctx)

    def visitExecCicsStatement(self, ctx):
        children_texts = [child.getText().upper() for child in ctx.children]
        if "XCTL" in children_texts and "PROGRAM" in children_texts:
            idx = children_texts.index("PROGRAM") + 1
            if idx < len(children_texts):
                call_text = self.clean_text(self.get_recursive_text(ctx.getChild(idx)))
                call_obj = { "description": call_text, "type": "execution", "comments": "" }
                if self.current_entry:
                    self.calls[self.current_entry].append(call_obj)
                    self.flow_calls[self.current_entry].append(call_obj)
                    # logger.info(f"[FLOW] {self.current_entry} → {call_obj}")
        if "RECEIVE" in children_texts and "INTO" in children_texts:
            idx = children_texts.index("INTO") + 1
            if idx < len(children_texts):
                input_text = self.clean_text(self.get_recursive_text(ctx.getChild(idx)))
                if self.current_entry:
                    self.entry_inputs[self.current_entry].append(input_text)
                    # logger.info(f"[INPUT] {self.current_entry} → {input_text}")
        if "SEND" in children_texts and "MAP" in children_texts:
            idx = children_texts.index("MAP") + 1
            if idx < len(children_texts):
                output_text = self.clean_text(self.get_recursive_text(ctx.getChild(idx)))
                if self.current_entry:
                    self.entry_outputs[self.current_entry].append(output_text)
                    # logger.info(f"[OUTPUT] {self.current_entry} → {output_text}")
        return self.visitChildren(ctx)

    def visitMoveStatement(self, ctx):
        if ctx.getChildCount() > 2:
            source = self.clean_text(self.get_recursive_text(ctx.getChild(1)))
            target = self.clean_text(self.get_recursive_text(ctx.getChild(3)))
            if self.current_entry and target.upper() == "WTO-MESSAGE":
                self.entry_outputs[self.current_entry].append(source)
                # logger.info(f"[OUTPUT] {self.current_entry} → {source}")
        return self.visitChildren(ctx)

    def visitReadStatement(self, ctx):
        children_texts = [child.getText().upper() for child in ctx.children]
        if "INTO" in children_texts:
            idx = children_texts.index("INTO") + 1
            if idx < len(ctx.children):
                input_text = self.clean_text(self.get_recursive_text(ctx.getChild(idx)))
                if self.current_entry:
                    self.entry_inputs[self.current_entry].append(input_text)
                    # logger.info(f"[INPUT] {self.current_entry} → {input_text}")
        return self.visitChildren(ctx)

    # --- Αναδρομική ανάλυση condition statements ---
    def parse_if_statement(self, text):
        """
        Μία απλή υλοποίηση για την ανάλυση ενός IF statement.
        Υποθέτουμε το format:
           IF <condition> MOVE ... [MOVE ...] ELSE MOVE ...
        όπου:
          - <condition> είναι το κομμάτι μέχρι το πρώτο " MOVE "
          - Το Then branch περιέχει τα MOVE statements πριν το " ELSE "
          - Το Else branch τα MOVE statements μετά το " ELSE "
        """
        upper_text = text.upper()
        if not upper_text.startswith("IF "):
            return None
        after_if = text[3:].strip()
        parts = after_if.split(" ELSE ", 1)
        then_part = parts[0].strip()
        else_part = parts[1].strip() if len(parts) > 1 else ""
        tokens = then_part.split(" MOVE ", 1)
        if len(tokens) < 2:
            return None
        condition_expr = tokens[0].strip()
        assignments_text = tokens[1].strip()
        then_assignments = []
        for part in assignments_text.split(" MOVE "):
            part = part.strip()
            if part:
                then_assignments.append("MOVE " + part)
        else_assignments = []
        if else_part:
            for part in else_part.split(" MOVE "):
                part = part.strip()
                if part:
                    else_assignments.append("MOVE " + part)
        return condition_expr, then_assignments, else_assignments

    def parse_statement_recursively(self, text):
        """
        Αναδρομικά αναλύει το text ενός statement.  
        Αν ξεκινάει με IF, καλεί την parse_if_statement και για κάθε assignment
        στο Then/Else καλεί αναδρομικά την ίδια συνάρτηση ώστε να "ξεδιπλωθεί" το condition.
        Διαφορετικά επιστρέφει ένα απλό αντικείμενο με type:
          - "Assingment" αν ξεκινάει με MOVE,
          - αλλιώς "execution".
        """
        text = self.clean_text(text)
        upper_text = text.upper()
        if upper_text.startswith("IF"):
            parsed = self.parse_if_statement(text)
            if parsed:
                condition_expr, then_assignments, else_assignments = parsed
                then_objs = []
                for assign_text in then_assignments:
                    then_objs.append(self.parse_statement_recursively(assign_text))
                else_objs = []
                for assign_text in else_assignments:
                    else_objs.append(self.parse_statement_recursively(assign_text))
                return {
                    "description": text,
                    "type": "condition",
                    "comments": "",
                    "Then": then_objs,
                    "Else": else_objs
                }
        if upper_text.startswith("MOVE"):
            st_type = "Assingment"
        else:
            st_type = "execution"
        return {
            "description": text,
            "type": st_type,
            "comments": ""
        }

    def visitStatement(self, ctx):
        if self.current_entry is not None:
            statement_text = self.clean_text(self.get_recursive_text(ctx))
            upper_text = statement_text.upper()
            if upper_text.startswith("IF") or upper_text.startswith("EVALUATE"):
                call_obj = self.parse_statement_recursively(statement_text)
            elif upper_text.startswith("MOVE"):
                call_obj = { "description": statement_text, "type": "ανάθεση", "comments": "" }
            elif "WHILE" in upper_text:
                call_obj = { "description": statement_text, "type": "loop", "comments": "" }
            else:
                call_obj = { "description": statement_text, "type": "execution", "comments": "" }
            self.flow_calls[self.current_entry].append(call_obj)
            # logger.info(f"[FLOW-CALL] {self.current_entry} → {call_obj}")
        return self.visitChildren(ctx)

    def generate_json_output(self):
        # Δημιουργούμε το αντικείμενο του προγράμματος και ενσωματώνουμε τις ιδιότητες από το Identification Division.
        program_obj = {}
        # Εισάγουμε κάθε property ξεχωριστά, αν υπάρχει
        for key in ["ProgramID", "AUTHOR", "INSTALLATION", "DATE-WRITTEN", "SECURITY"]:
            if key in self.identification_properties:
                program_obj[key] = self.identification_properties[key]
        program_obj["EntryPoints"] = []
        for entry in self.entry_points:
            entry_data = {
                "Name": entry,
                "Input": self.entry_inputs.get(entry, []),
                "Output": self.entry_outputs.get(entry, []),
                "Calls": self.flow_calls.get(entry, [])
            }
            program_obj["EntryPoints"].append(entry_data)
        json_output = {"Program": program_obj}
        json_str = json.dumps(json_output, indent=4)
        logger.info("==== GENERATED JSON ====")
        logger.info(json_str)
        return json_str

    def print_json_output(self):
        return self.generate_json_output()
