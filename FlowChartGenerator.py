import json

class FlowChartGenerator:
    def __init__(self):
        pass
    
    def process_sentence(self, sentence, indent=0):
        """
        Επεξεργάζεται μια "Sentence" και επιστρέφει μια λίστα με τις γραμμές PlantUML.
        """
        lines = []
        prefix = "  " * indent
        stype = sentence.get("type", "")

        if stype == "StatementType.ASSIGN":
            assign_from = sentence.get("AssignFrom", "")
            assign_to = sentence.get("AssignTo", "")
            lines.append(prefix + f":Assign {assign_from} to {assign_to};")
        elif stype == "StatementType.CALL":
            method = sentence.get("MethodName", "")
            lines.append(prefix + f":Call {method};")
        elif stype == "StatementType.CICS":
            method = sentence.get("MethodName", "")
            # Επεξεργασία τυχόν παραμέτρων/υποεντολών
            params = []
            for s in sentence.get("Statements", []):
                params.append(s.get("type", ""))
            params_str = " ".join(params)
            lines.append(prefix + f":CICS {method} {params_str};")
        elif stype == "StatementType.CONDITION":
            # Στην περίπτωση της συνθήκης, θεωρούμε ότι δεν υπάρχουν συγκεκριμένες συνθήκες (conditionClauses)
            lines.append(prefix + "if (Condition) then (true)")
            for s in sentence.get("TrueStatements", []):
                lines.extend(self.process_sentence(s, indent+1))
            if sentence.get("FalseStatements"):
                lines.append(prefix + "else (false)")
                for s in sentence.get("FalseStatements", []):
                    lines.extend(self.process_sentence(s, indent+1))
            lines.append(prefix + "endif")
        else:
            # Αν δεν ταιριάζει κάποια από τις παραπάνω περιπτώσεις
            lines.append(prefix + f":{stype};")
        return lines

    def process_flow(self, flow):
        """
        Επεξεργάζεται μια ροή (flow) και επιστρέφει το πλήρες PlantUML διάγραμμα ως λίστα γραμμών.
        """
        lines = []
        lines.append("@startuml")
        lines.append("start")
        for sentence in flow.get("Sentences", []):
            lines.extend(self.process_sentence(sentence))
        lines.append("stop")
        lines.append("@enduml")
        return lines
    
    def genrateDiagram(self, json_data, entry_point):
        print(json_data.to_json())
        data = json.loads(json_data.to_json())
        # Επιλέγουμε για το παράδειγμα τη ροή "00000-MAIN"
        main_flow = None
        for flow in data.get("Flow", []):
            if flow.get("Name") == entry_point:
                main_flow = flow
                break

        if main_flow:
            plantuml_lines = self.process_flow(main_flow)
            plantuml_code = "\n".join(plantuml_lines)
            print(plantuml_code)
        else:
            print(f"Η ροή {entry_point} δεν βρέθηκε στο JSON.")