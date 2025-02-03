from grammars.Cobol85Visitor import Cobol85Visitor
from grammars.Cobol85Parser import Cobol85Parser
import re

class CobolVisitor(Cobol85Visitor):
    def __init__(self):
        self.programs = []
        self.entry_points = []
        self.entry_inputs = {}
        self.calls = []
        self.variables = []
        self.current_entry = None

    def visitProgramIdParagraph(self, ctx):
        """
        Εντοπισμός του κύριου προγράμματος COBOL, ακόμα και αν προηγούνται σχόλια.
        """
        found_program_id = False
        program_name = None
        # print(ctx.getText())
        for i in range(ctx.getChildCount()):
            # a = ctx.getChild(i).getText()
            # print(a)
            if ctx.getChild(i).getText().upper() == "PROGRAM-ID":
                if i + 1 < ctx.getChildCount():
                    program_name = ctx.getChild(i + 1).getText()
                    found_program_id = True
                break

        if found_program_id and program_name:
            self.programs.append(program_name)
            self.entry_points.append(program_name)
            self.entry_inputs[program_name] = []
            self.current_entry = program_name
            # print(f"[ENTRY POINT] Program Name: {program_name}")
        else:
            print("[ERROR] Program-ID not found!")

        return self.visitChildren(ctx)


    def visitCallStatement(self, ctx):
        """
        Ανάλυση CALL USING: Εντοπισμός μεταβλητών που χρησιμοποιούνται ως input.
        """
        if ctx.getChildCount() > 2:
            called_program = ctx.getChild(2).getText()
            inputs = []

            found_using = False
            for child in ctx.children:
                if child.getText().upper() == "USING":
                    found_using = True
                elif found_using and hasattr(child, 'getText') and child.getText().isidentifier():
                    inputs.append(child.getText())

            if self.current_entry:
                if self.current_entry in self.entry_inputs:
                    self.entry_inputs[self.current_entry].extend(inputs)
                else:
                    self.entry_inputs[self.current_entry] = inputs

            # print(f"[CALL DETECTED] {self.current_entry} calls {called_program} USING {inputs}")

        return self.visitChildren(ctx)


    def visitParagraph(self, ctx):
        """
        Εντοπισμός παραγράφων στη PROCEDURE DIVISION ως τοπικά entry points.
        """
        if ctx.getChildCount() > 0:
            paragraph_name = ctx.getChild(0).getText()
            self.entry_points.append(paragraph_name)
            self.entry_inputs[paragraph_name] = []
            self.current_entry = paragraph_name
            # print(f"[LOCAL ENTRY] Paragraph: {paragraph_name}")
        return self.visitChildren(ctx)


    def visitMoveStatement(self, ctx):
        """
        Ανάλυση MOVE statement: Προσθήκη μεταβλητών που χρησιμοποιούνται ως inputs.
        """
        move_source = None
        move_target = None

        tokens = [child.getText() for child in ctx.children]

        # Διόρθωση προβλήματος συγχωνευμένων tokens με regex
        for token in tokens:
            match = re.match(r"(.+?)TO(.+)", token, re.IGNORECASE)
            if match:
                move_source, move_target = match.groups()

        if move_source and move_target and self.current_entry:
            if self.current_entry in self.entry_inputs:
                self.entry_inputs[self.current_entry].append(move_source)
            else:
                self.entry_inputs[self.current_entry] = [move_source]

            # print(f"[MOVE INPUT] {self.current_entry} moves {move_source} into {move_target}")

        return self.visitChildren(ctx)






    def visitDataDescriptionEntry(self, ctx):
        """
        Εντοπισμός μεταβλητών από τη DATA DIVISION.
        """
        variable_name = None

        # Ελέγχουμε αν υπάρχει `dataDescriptionEntryFormat1`
        if ctx.dataDescriptionEntryFormat1():
            format1 = ctx.dataDescriptionEntryFormat1()
            a = format1.dataPictureClause()
            # print(">>>>>>>>>>>>>>>>>>> format1 >>>>>>>>>>>>>>>>>>>>>>>>")
            # print(format1)
            # print("<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<")
            if format1.dataTypeClause():
                picture_clause = format1.dataTypeClause()
                # print("==================picture_clause=")
                # print(picture_clause)
            if format1.dataName():
                variable_name = format1.dataName().getText()

        # Ελέγχουμε αν υπάρχει `dataDescriptionEntryFormat2` (RENAME)
        elif ctx.dataDescriptionEntryFormat2():
            format2 = ctx.dataDescriptionEntryFormat2()
            if format2.dataName():
                variable_name = format2.dataName().getText()

        # Ελέγχουμε αν υπάρχει `dataDescriptionEntryFormat3` (LEVEL 88)
        elif ctx.dataDescriptionEntryFormat3():
            format3 = ctx.dataDescriptionEntryFormat3()
            if format3.conditionName():
                variable_name = format3.conditionName().getText()

        if variable_name:
            self.variables.append(variable_name)
            # print(f"[VARIABLE] Found: {variable_name}")
        else:
            print(f"[ERROR] Could not determine variable name. Context: {ctx.toStringTree()}")

        return self.visitChildren(ctx)

    # def visitDataDescriptionEntry(self, ctx):
    #     """
    #     Εντοπισμός μεταβλητών από τη DATA DIVISION.
    #     """
    #     variable_name = None
    #     data_type = None
    #     size = None
    #     initial_value = None

    #     # Ελέγχουμε αν υπάρχει `dataDescriptionEntryFormat1`
    #     if ctx.dataDescriptionEntryFormat1():
    #         format1 = ctx.dataDescriptionEntryFormat1()

    #         # Όνομα μεταβλητής
    #         if format1.dataName():
    #             variable_name = format1.dataName().getText()

    #         # Έλεγχος για τύπο δεδομένων (PICTURE)
    #         if format1.dataPictureClause():
    #             picture_clause = format1.dataPictureClause()
    #             print("==================picture_clause=")
    #             print(picture_clause)
    #             data_type = picture_clause.getText()

    #         # Έλεγχος για μέγεθος μεταβλητής (PICTURE Χαρακτήρες)
    #         if format1.dataPictureClause() and format1.dataPictureClause().pictureString():
    #             size = format1.dataPictureClause().pictureString().getText()

    #         # Έλεγχος για αρχική τιμή (VALUE)
    #         if format1.dataValueClause():
    #             value_clause = format1.dataValueClause()
    #             initial_value = value_clause.getText()

    #     # Εκτύπωση αποτελεσμάτων
    #     if variable_name:
    #         print(f"[VARIABLE] Found: {variable_name}")
    #         if data_type:
    #             print(f"  ├── Type: {data_type}")
    #         if size:
    #             print(f"  ├── Size: {size}")
    #         if initial_value:
    #             print(f"  ├── Initial Value: {initial_value}")
    #     else:
    #         print(f"[ERROR] Could not determine variable name. Context: {ctx.toStringTree()}")

    #     return self.visitChildren(ctx)




    def visitLinkageSection(self, ctx):
        """
        Εντοπισμός παραμέτρων που δηλώνονται στο LINKAGE SECTION.
        """
        linkage_vars = [child.getText() for child in ctx.children if child.getText().isidentifier()]
        if self.current_entry:
            if self.current_entry in self.entry_inputs:
                self.entry_inputs[self.current_entry].extend(linkage_vars)
            else:
                self.entry_inputs[self.current_entry] = linkage_vars
            # print(f"[ENTRY INPUTS] {self.current_entry} expects {linkage_vars}")
        return self.visitChildren(ctx)

    def print_results(self):
        """
        Εκτυπώνει τα αποτελέσματα της ανάλυσης.
        """
        print("===========================================================")
        print("Programs found:", self.programs)
        print("Entry Points found:", self.entry_points)
        print("Calls found:", self.calls)
        print("Variables found:", self.variables)
        print("Inputs per Entry Point:")
        for entry, inputs in self.entry_inputs.items():
            print(f"  {entry}: {inputs}")
        print("-----------------------------------------------------------")
        
            
    def visitReadStatement(self, ctx):
        """
        Ανάλυση READ INTO: Εντοπισμός μεταβλητής που χρησιμοποιείται ως input.
        """
        if "INTO" in [child.getText().upper() for child in ctx.children]:
            idx = [child.getText().upper() for child in ctx.children].index("INTO") + 1
            if idx < len(ctx.children):
                input_variable = ctx.getChild(idx).getText()
                if self.current_entry:
                    if self.current_entry in self.entry_inputs:
                        self.entry_inputs[self.current_entry].append(input_variable)
                    else:
                        self.entry_inputs[self.current_entry] = [input_variable]

                    print(f"[READ INPUT] {self.current_entry} reads into {input_variable}")

        return self.visitChildren(ctx)

    def visitAcceptStatement(self, ctx):
        """
        Ανάλυση ACCEPT FROM: Βρίσκουμε μεταβλητές που δέχονται input από το χρήστη.
        """
        if ctx.getChildCount() > 1:
            input_variable = ctx.getChild(1).getText()
            if self.current_entry:
                if self.current_entry in self.entry_inputs:
                    self.entry_inputs[self.current_entry].append(input_variable)
                else:
                    self.entry_inputs[self.current_entry] = [input_variable]

                # print(f"[ACCEPT INPUT] {self.current_entry} accepts {input_variable}")

        return self.visitChildren(ctx)
