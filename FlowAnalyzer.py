from collections import defaultdict
from grammars.Cobol85Visitor import Cobol85Visitor

class FlowAnalyzer(Cobol85Visitor):
    def __init__(self):
        super().__init__()
        # Χρησιμοποιούμε defaultdict για να αποφύγουμε ελέγχους ύπαρξης κλειδιών
        self.flow_graph: defaultdict[str, list[str]] = defaultdict(list)  # {entry_point: [next_steps]}
        self.outputs: defaultdict[str, list[str]] = defaultdict(list)       # {entry_point: [displayed_outputs]}
        self.current_entry: str | None = None

    def add_edge(self, source: str, target: str) -> None:
        """Προσθέτει ένα edge στο flow graph από το source στο target."""
        self.flow_graph[source].append(target)

    def add_output(self, entry: str, output: str) -> None:
        """Καταγράφει το output που παράγεται για ένα συγκεκριμένο entry point."""
        self.outputs[entry].append(output)

    def visitParagraph(self, ctx) -> any:
        """
        Όταν εντοπίζεται μια νέα παράγραφος, 
        ορίζουμε το πρώτο στοιχείο της ως το current entry point.
        """
        if ctx.getChildCount() > 0:
            entry_point = ctx.getChild(0).getText()
            self.current_entry = entry_point
            # Ακόμα κι αν δεν προστεθούν edges, διατηρούμε το entry στο flow_graph
            _ = self.flow_graph[entry_point]
            print(f"[ENTRY] {entry_point}")
        return self.visitChildren(ctx)

    def visitPerformStatement(self, ctx) -> any:
        """
        Ανάλυση του PERFORM statement για την καταγραφή του flow:
        Προσθέτει μια ακμή (edge) από το τρέχον entry στο παρασχολόμενο paragraph.
        """
        if ctx.getChildCount() > 1:
            performed_paragraph = ctx.getChild(1).getText()
            if self.current_entry:
                self.add_edge(self.current_entry, performed_paragraph)
                print(f"\t[FLOW] {self.current_entry} → {performed_paragraph}")
        return self.visitChildren(ctx)

    def visitDisplayStatement(self, ctx) -> any:
        """
        Ανάλυση του DISPLAY statement για την καταγραφή του output.
        """
        output_text = ctx.getChild(1).getText() if ctx.getChildCount() > 1 else "UNKNOWN"
        if self.current_entry:
            self.add_output(self.current_entry, output_text)
            print(f"[OUTPUT] {self.current_entry} → {output_text}")
        return self.visitChildren(ctx)

    def print_flow(self) -> None:
        """Εκτυπώνει το flow graph και τα outputs ανά entry point."""
        print("\nFlow Graph:")
        for entry, steps in self.flow_graph.items():
            print(f"  {entry} → {steps}")

        print("\nOutputs per Entry Point:")
        for entry, outputs in self.outputs.items():
            print(f"  {entry}: {outputs}")
