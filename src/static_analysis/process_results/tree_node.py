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
    
    def add_child(self, child):
        child.parent = self
        self.children.append(child) 
    
    def print_tree(self, level=0):
        nodes = []
        nodes.append(f'| {self.kindof:<30} | weight: {self.weight} | level: {level} | {"\t" * level}    {self.methodName}')
        # print('\t' * level + self.node_name)
        for child in self.children:
            nodes.extend(child.print_tree(level + 1))
        return nodes