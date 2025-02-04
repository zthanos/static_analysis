from grammars.Cobol85Visitor import Cobol85Visitor

class FlowAnalyzer(Cobol85Visitor):
    def __init__(self):
        super().__init__()
        self.flow_graph = {}  # {entry_point: [next_steps]}
        self.outputs = {}     # {entry_point: [displayed_outputs]}
        self.current_entry = None

    def add_edge(self, source, target):
        """Προσθέτει ένα edge στο flow graph."""
        if source in self.flow_graph:
            self.flow_graph[source].append(target)
        else:
            self.flow_graph[source] = [target]

    def add_output(self, entry, output):
        """Καταγράφει το output που παράγεται σε κάθε entry point."""
        if entry in self.outputs:
            self.outputs[entry].append(output)
        else:
            self.outputs[entry] = [output]

    def getRecursiveText(self, ctx):
        """
        Αναδρομικά συλλέγει το κείμενο από τον κόμβο και τους απογόνους του,
        διατηρώντας τα διαστήματα μεταξύ των μερών.
        """
        if ctx.getChildCount() == 0:
            return ctx.getText()
        else:
            parts = []
            for i in range(ctx.getChildCount()):
                child = ctx.getChild(i)
                parts.append(self.getRecursiveText(child))
            # Ενώνουμε τα κομμάτια με ένα κενό
            return " ".join(parts)

    def visitParagraph(self, ctx):
        """Καταγράφει μια νέα παράγραφο ως entry point."""
        if ctx.getChildCount() > 0:
            self.current_entry = ctx.getChild(0).getText()
            if self.current_entry not in self.flow_graph:
                self.flow_graph[self.current_entry] = []
            print(f"[ENTRY] {self.current_entry}")
        return self.visitChildren(ctx)

    def visitPerformStatement(self, ctx):
        """
        Επεξεργάζεται το PERFORM statement ώστε να καταγραφεί το flow.
        Η συλλογή του κειμένου γίνεται αναδρομικά ώστε να διατηρηθούν τα κενά.
        """
        if ctx.getChildCount() > 1:
            performed_paragraph = self.getRecursiveText(ctx.getChild(1)).strip()
            if self.current_entry:
                self.add_edge(self.current_entry, performed_paragraph)
                print(f"\t[FLOW] {self.current_entry} → {performed_paragraph}")
        return self.visitChildren(ctx)

    def visitDisplayStatement(self, ctx):
        """
        Επεξεργάζεται το DISPLAY statement ώστε να καταγραφεί το output.
        Η συλλογή του κειμένου γίνεται αναδρομικά ώστε να διατηρηθούν τα κενά.
        """
        if ctx.getChildCount() > 1:
            output_text = self.getRecursiveText(ctx.getChild(1)).strip()
        else:
            output_text = "UNKNOWN"
        if self.current_entry:
            self.add_output(self.current_entry, output_text)
            print(f"[OUTPUT] {self.current_entry} → {output_text}")
        return self.visitChildren(ctx)

    def visitEvaluateStatement(self, ctx):
        """
        Επεξεργάζεται το EVALUATE statement ώστε να καταγραφεί το flow.
        Συλλέγει ολόκληρο το Evaluate statement (συμπεριλαμβανομένων των When clauses)
        σε μία συμβολοσειρά και το προσθέτει ως edge, χωρίς να επισκέπτεται περαιτέρω τα παιδιά.
        """
        parts = []
        # Διατρέχουμε όλα τα παιδιά του Evaluate statement
        for i in range(ctx.getChildCount()):
            child = ctx.getChild(i)
            child_text = self.getRecursiveText(child).strip()
            # Εάν το κομμάτι ξεκινά με "WHEN" ή "WHEN OTHER", προσθέτουμε νέα γραμμή και εσοχή
            if child_text.startswith("WHEN"):
                parts.append("\n\t" + child_text)
            else:
                parts.append(child_text)
        evaluate_text = " ".join(parts)
        evaluate_text = evaluate_text.replace("PERFORM", " PERFORM")
        if self.current_entry:
            self.add_edge(self.current_entry, evaluate_text)
            print(f"\t[FLOW] {self.current_entry} → {evaluate_text}")
        # Μην επισκεπτόμαστε περαιτέρω τα παιδιά, για να μην επαναλαμβάνουμε τα when clauses ως ξεχωριστά edges.
        return None

    def visitWhenClause(self, ctx):
        """
        Εάν τα When clauses εμφανίζονται μέσα σε Evaluate, δεν θέλουμε να τα
        προσθέτουμε ξεχωριστά, οπότε παραλείπονται.
        """
        # Απλά επιστρέφουμε χωρίς να κάνουμε further processing.
        return None

    def visitWhenOtherClause(self, ctx):
        """
        Εάν το When Other clause εμφανίζεται μέσα σε Evaluate, δεν θέλουμε να το
        προσθέτουμε ξεχωριστά, οπότε παραλείπεται.
        """
        return None

    def print_flow(self):
        """Εκτυπώνει το flow graph και τα outputs ανά entry point."""
        print("\nFlow Graph:")
        for entry, steps in self.flow_graph.items():
            print(f"  {entry} → {steps}")

        print("\nOutputs per Entry Point:")
        for entry, outputs in self.outputs.items():
            print(f"  {entry}: {outputs}")
