from logger import logger 

EXTERNAL_CALL_RATE = 1.0
INTERNAL_CALL_RATE = 0.5
CONDITIONAL_RATE = 0.4
ASSINGMENT_RATE = 0.3
OTHER_RATE = 0.2
class TreeNode:
    def __init__(self, data, kindof=None):
        self.children = []
        if isinstance(data, dict) and data.get('methodName') is not None:
            self.methodName = data.get('methodName')
            self.id = data.get('id')
            self.type = data.get('type')
            match self.type:
                case 'StatementType.CALL':
                    self.weight = EXTERNAL_CALL_RATE if data.get('internal') == False else INTERNAL_CALL_RATE
                    self.kindof = 'External call' if data.get('internal') == False else 'Internal call'
                case 'StatementType.CONDITION':
                    self.weight = CONDITIONAL_RATE
                    self.kindof = 'Conditional' 
                case 'StatementType.ASSIGN':
                    self.weight = ASSINGMENT_RATE
                    self.kindof = 'Assignment' 
                case 'StatementType.OTHER':
                    self.weight = OTHER_RATE
                    self.kindof = 'Other'       
        else:
            self.methodName = data
            self.id = None
            self.type = "StatementType.ENTRY_POINT"
            self.weight = 1.0
            self.kindof = 'Entry Point'            
        
        self.kindof = f'{kindof:<10} - {self.kindof}' if kindof != None else self.kindof                                  
        
        self.parent = None
    def true_path(self):
        path = []
        if self.type == 'StatementType.CONDITION':
            for child in self.children:
                if child.kindof.startswith('True Path'):
                    path.append(child.methodName)
        return path
    
    def path(self):
        path = []
        for child in self.children:
            if child.kindof.startswith('True Path'):
                path.append(child.methodName)
        return path
    def false_path(self):
        path = []
        if self.type == 'StatementType.CONDITION':
            for child in self.children:
                if child.kindof.startswith('False Path'):
                    path.append(child.methodName)
        return path
    
    def add_child(self, child):
        child.parent = self
        self.children.append(child) 
    
    def print_tree(self, level=0):
        nodes = []
        nodes.append(f'|weight: {self.weight:>6} | level: {level:>3} | {"\t" * level}    {self.methodName}')
        # print('\t' * level + self.node_name)
        for child in self.children:
            nodes.extend(child.print_tree(level + 1))
        return nodes
    
    # def get_conditions(self, level=0):
    #     nodes = []
    #     if self.type == 'StatementType.CONDITION':
    #         nodes.extend([True])
    #     # print('\t' * level + self.node_name)
    #     for child in self.children:
    #         nodes.extend(child.get_conditions(level + 1))
    #     return nodes        
    
    def get_conditions_structure(self):
        result = []
        
        if not self.children:
            return result
        
        # Add current level's branches
        current_level = []
        for child in self.children:
            if child.type == 'StatementType.CONDITION':  # or whatever marks a condition
                current_level.append(True)
        
        if current_level:
            result.append(current_level)
        
        # Process children's structures
        for child in self.children:
            child_structure = child.get_conditions_structure()
            for i, level in enumerate(child_structure):
                if i < len(result) - 1:
                    result[i + 1].extend(level)
                else:
                    result.append(level)
        
        return result    
    
    def get_flat_conditions_structure(self):
        result = []

        def traverse(node):
            if getattr(node, 'type', None) == 'StatementType.CONDITION':
                result.append(True)
            for child in getattr(node, 'children', []):
                traverse(child)

        traverse(self)
        return result
        
    
    
    
    def get_conditions_grouped_by_level(self, level=0, grouped=None):
        if grouped is None:
            grouped = []

        if len(grouped) <= level:
            grouped.append([])

        if self.type == 'StatementType.CONDITION':
            grouped[level].append(True)

        for child in self.children:
            child.get_conditions_grouped_by_level(level + 1, grouped)

        return grouped
    
    def print_paths(self, level=0):
        nodes = []
        nodes.append(self.paths())
        return nodes    


    # def get_condition_paths(self, current_path=None):
    #     if self.kindof.startswith('True Path'):
    #         if 
    #         return []
        
        
        
        
    def get_all_paths(self, current_path=None):
        if current_path is None:
            current_path = []

        paths = []

        # Αγνόησε τα True/False/Entry point nodes στο path
        if self.kindof not in ["True Path", "False Path", "Entry Point"]:
            current_path = current_path + [self.methodName]

        # Αν το node δεν έχει παιδιά, πήγαινε στο επόμενο sibling ή τελείωσε το path
        if not self.children:
            sibling = self.get_next_sibling()
            if sibling:
                return sibling.get_all_paths(current_path)
            else:
                return [current_path]

        # Αν είναι condition, χειρίσου αναδρομικά τα True/False branches
        if self.type == "StatementType.CONDITION":
            for path_branch in self.children:  # True/False branches
                branch_paths = [current_path]  # Αρχικοποίηση κάθε branch με το current_path
                for child in path_branch.children:
                    new_branch_paths = []
                    for path in branch_paths:
                        child_paths = child.get_all_paths(path)
                        new_branch_paths.extend(child_paths)
                    branch_paths = new_branch_paths

                # Μετά από το branch, έλεγξε για sibling και συνέχισε από εκεί
                sibling = self.get_next_sibling()
                if sibling:
                    for branch_path in branch_paths:
                        paths.extend(sibling.get_all_paths(branch_path))
                else:
                    paths.extend(branch_paths)
        else:
            # Αν δεν είναι condition, συνέχισε κανονικά με τα παιδιά
            child_paths = [current_path]
            for child in self.children:
                new_child_paths = []
                for path in child_paths:
                    child_paths_extended = child.get_all_paths(path)
                    new_child_paths.extend(child_paths_extended)
                child_paths = new_child_paths

            # Συνέχισε με sibling αν υπάρχει
            sibling = self.get_next_sibling()
            if sibling:
                for path in child_paths:
                    paths.extend(sibling.get_all_paths(path))
            else:
                paths.extend(child_paths)

        return paths

    # Μέθοδος εύρεσης επόμενου sibling
    def get_next_sibling(self):
        if self.parent is None:
            return None
        siblings = self.parent.children
        idx = siblings.index(self)
        return siblings[idx + 1] if idx + 1 < len(siblings) else None




    