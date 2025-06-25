# from logger import logger
# from tree_node import TreeNode
# from itertools import product

# class MyTree:
#     def __init__(self):
#         self.root=None
#     def add_root(self, data, kindof=None):
#         if isinstance(data, TreeNode):
#             self.root = data
#             return self.root
#         if self.root is None:
#             self.root = TreeNode(data, kindof)
#             return self.root
#         # else:
#         #     return self.root.add_child(data)
        
#     def add_child(self, data):
#         return self.root.add_child(data)
    
#     def print_tree(self):
#         tree_data = self.root.print_tree()
#         for node in tree_data:
#             logger.info(node)
    
#     def get_node_by_name(self, name):
#         # locate a node by name in all nodes starting from the root
#         def _get_node_by_name(node, name):
#             if node.methodName == name:
#                 return node
#             for child in node.children:
#                 result = _get_node_by_name(child, name)
#                 if result:
#                     return result
#             return None
        
#         return _get_node_by_name(self.root, name)


#     def get_condition_paths(self, node, path):
#         search_path = "True Path" if path else "False Path"
#         condition_path = [child for child in node.children if child.methodName == search_path]
#         if not condition_path:
#             return []
#         branch_true =[]
#         branch_true.append(f'{node.methodName}({path})')
#         for st in condition_path[0].children:
#             if st.type=="StatementType.CONDITION":
#                 branch_true.extend(self.get_path(st, search_path))
#             else:
#                 branch_true.append(st.methodName)
#             # logger.info(st.methodName)        
#         return branch_true
    
#     def get_path(self, node, path=True):
#         branch =[]
#         if node.type=="StatementType.CONDITION":
#             branch.extend(self.get_condition_paths(node, path))
#         else:
#             branch.append(node.methodName)
#         return branch
                    

    
#     def define_possible_paths(self):
#         pos_path = self.root.get_flat_conditions_structure()
#         return self.generate_all_flat_combinations(pos_path)

#     def print_paths(self, condition_branching, node  = None):
#         if node is None:
#             node = self.root
#         paths = []
#         if not condition_branching:
#             condition_branching=[]

#         np = []
#         for index, statement in enumerate(node.children):
#             if statement.type=="StatementType.CONDITION":
#                 if index > len(condition_branching) -1:
#                     condition_branching.append(True)
#                 else:
#                     condition_branching[index]=not condition_branching[index]
#                 condition_path = condition_branching[index]
#                 np.extend(self.get_condition_paths(condition_path, statement))
#             else:
#                 np.append(statement.methodName)
#         for np in paths:
#                 logger.info("  ".join(p))
#         paths.append(np)
#         self.print_paths(node, 0, condition_branching)
#         for p in paths:
#             logger.info("  ".join(p))
#         a=1

#     def generate_all_combinations(self, condition_branching):
#         # Convert the input to a mutable structure (list of lists)
#         original = [list(level) for level in condition_branching]
#         combinations = []
        
#         def backtrack(current, level, index):
#             # Add the current state to combinations
#             combinations.append([list(level) for level in current])
            
#             # Work backwards through levels
#             for l in range(len(current)-1, level-1, -1):
#                 # Work backwards through conditions in each level
#                 for i in range(len(current[l])-1, index-1, -1):
#                     if current[l][i]:  # If it's True, flip it to False
#                         # Create new version with this condition flipped
#                         new_version = [list(lev) for lev in current]
#                         new_version[l][i] = False
                        
#                         # Continue backtracking from this position
#                         backtrack(new_version, l, i)
        
#         # Start with the original all-True version
#         backtrack(original, 0, 0)
        
#         # Remove duplicates while preserving order
#         seen = set()
#         unique_combinations = []
#         for combo in combinations:
#             tuple_version = tuple(tuple(level) for level in combo)
#             if tuple_version not in seen:
#                 seen.add(tuple_version)
#                 unique_combinations.append(combo)
        
#         return unique_combinations
    
    
    
#     def get_unique_paths(self, all_combinations):
#         seen_paths = set()
#         unique_paths = []
        
#         for combo in all_combinations:
#             # Convert the path to a string for easy comparison
#             path_str = " -> ".join(combo)
            
#             # Only add if we haven't seen this path before
#             if path_str not in seen_paths:
#                 seen_paths.add(path_str)
#                 unique_paths.append(combo)
        
#         return unique_paths    
    
#     def get_unique_paths_with_conditions(self):
#         path_map = {}
        
#         combs = self.define_possible_paths()
        
#         for combo in combs:
#             path = self.retrieve_path_flat(combo)

#             # Δημιουργία key για path_str από περιγραφή + condition    
#             path_key = tuple(
#                 (step['Description'], step.get('ConditionValue')) for step in path
#             )            
           
#     # Αν το path δεν έχει ήδη καταχωρηθεί, πρόσθεσέ το
#             if path_key not in path_map:
#                 path_map[path_key] = {
#                     'condition': combo,
#                     'path': path
#                 }
            
#         # Extract the unique paths with their representative conditions
#         unique_results = list(path_map.values())
#         return unique_results    

#     def extract_node_info(self, node, decision=None):
#         isExternal = node.kindof == 'External call'
#         return {'Description': node.methodName, 'Type': node.type, 'Weight': node.weight, 'ConditionValue': decision, 'External': isExternal, 'ExternalSystem':'CICS'}
    
#     def retrieve_path_flat(self, condition_sequence, node=None, condition_index=0):
#         path = []

#         if node is None:
#             node = self.root

#         # Προσθέτουμε το τρέχον node
#         path.append(self.extract_node_info(node))
#         # path.append(node.methodName)

#         for child in node.children:
#             if getattr(child, 'type', None) == "StatementType.CONDITION":
#                 if condition_index < len(condition_sequence):
#                     decision = condition_sequence[condition_index]
#                     condition_index += 1

#                     # path.append(f"{child.methodName}({decision})")
#                     path.append(self.extract_node_info(child, decision))
                    
#                     # Εύρεση True/False Path
#                     for branch in child.children:
#                         if decision and "True Path" in branch.methodName:
#                             path.extend(self.retrieve_path_flat(condition_sequence, branch, condition_index))
#                             break
#                         elif not decision and "False Path" in branch.methodName:
#                             path.extend(self.retrieve_path_flat(condition_sequence, branch, condition_index))
#                             break
#                 else:
#                     # Δεν υπάρχει άλλη πληροφορία από το sequence
#                     # path.append(child.methodName)
#                     path.append(self.extract_node_info(child))
                    
#                     for branch in child.children:
#                         path.extend(self.retrieve_path_flat(condition_sequence, branch, condition_index))
#             else:
#                 # Κανονικό statement: επεξεργασία κανονική
#                 # path.append(child.methodName)
#                 path.append(self.extract_node_info(child))                                    
                
#                 for grandchild in child.children:
#                     path.extend(self.retrieve_path_flat(condition_sequence, grandchild, condition_index))

#         return path

    def generate_all_flat_combinations(self, flat_condition_structure):
        # Δημιουργεί όλους τους πιθανούς συνδυασμούς True/False για τις θέσεις της flat λίστας
        return [list(combo) for combo in product([True, False], repeat=len(flat_condition_structure))]


###################################
# OTPIMIZED VERSION
####################################

from logger import logger
from tree_node import TreeNode
from itertools import product
from functools import lru_cache

class MyTree:
    def __init__(self):
        self.root = None
        self._node_cache = {}  # Cache for node lookups

    def add_root(self, data, kindof=None):
        if isinstance(data, TreeNode):
            self.root = data
            return self.root
        if self.root is None:
            self.root = TreeNode(data, kindof)
            return self.root
        
    def add_child(self, data):
        return self.root.add_child(data)
    
    def print_tree(self):
        tree_data = self.root.print_tree()
        for node in tree_data:
            logger.info(node)
    
    def get_node_by_name(self, name):
        # Check cache first
        if name in self._node_cache:
            return self._node_cache[name]
            
        # Iterative DFS to find node
        stack = [self.root]
        while stack:
            node = stack.pop()
            if node.methodName == name:
                self._node_cache[name] = node  # Cache the result
                return node
            # Add children in reverse order to process left-to-right
            stack.extend(reversed(node.children))
        
        return None

    def get_condition_paths(self, node, path):
        search_path = "True Path" if path else "False Path"
        # Find first matching child (using next() for efficiency)
        condition_path = next((child for child in node.children if child.methodName == search_path), None)
        if not condition_path:
            return []
            
        branch_true = [f'{node.methodName}({path})']
        for st in condition_path.children:
            if st.type == "StatementType.CONDITION":
                branch_true.extend(self.get_path(st, search_path))
            else:
                branch_true.append(st.methodName)
        return branch_true
    
    def get_path(self, node, path=True):
        if node.type == "StatementType.CONDITION":
            return self.get_condition_paths(node, path)
        return [node.methodName]
    
    def define_possible_paths(self):
        pos_path = self.root.get_flat_conditions_structure()
        return list(product([True, False], repeat=len(pos_path)))

    def extract_node_info(self, node, decision=None):
        return {
            'Description': node.methodName,
            'Type': node.type,
            'Weight': node.weight,
            'ConditionValue': decision,
            'External': node.kindof == 'External call',
            'ExternalSystem': 'CICS'
        }

    def retrieve_path_flat(self, condition_sequence, node=None, condition_index=0):
        path = []
        if node is None:
            node = self.root

        path.append(self.extract_node_info(node))

        for child in node.children:
            if getattr(child, 'type', None) == "StatementType.CONDITION":
                if condition_index < len(condition_sequence):
                    decision = condition_sequence[condition_index]
                    condition_index += 1
                    path.append(self.extract_node_info(child, decision))
                    
                    # Find matching branch
                    branch_type = "True Path" if decision else "False Path"
                    matching_branch = next(
                        (b for b in child.children if branch_type in b.methodName),
                        None
                    )
                    if matching_branch:
                        path.extend(self.retrieve_path_flat(
                            condition_sequence, 
                            matching_branch, 
                            condition_index
                        ))
                else:
                    path.append(self.extract_node_info(child))
                    for branch in child.children:
                        path.extend(self.retrieve_path_flat(
                            condition_sequence, 
                            branch, 
                            condition_index
                        ))
            else:
                path.append(self.extract_node_info(child))                                    
                for grandchild in child.children:
                    path.extend(self.retrieve_path_flat(
                        condition_sequence, 
                        grandchild, 
                        condition_index
                    ))
        return path

    # def get_unique_paths_with_conditions(self):
    #     path_map = {}
    #     combs = self.define_possible_paths()
        
    #     for combo in combs:
    #         path = self.retrieve_path_flat(combo)
    #         path_key = tuple(
    #             (step['Description'], step.get('ConditionValue')) 
    #             for step in path
    #         )
            
    #         if path_key not in path_map:
    #             path_map[path_key] = {
    #                 'condition': combo,
    #                 'path': path
    #             }
        
    #     return list(path_map.values())
    
    
    @lru_cache(maxsize=None)
    def _cached_retrieve_path_flat(self, combo_tuple):
        # επειδή το combo είναι πιθανόν λίστα ή dict, μετατρέπουμε σε tuple ώστε
        # να μπορεί να cache-αριστεί
        combo = list(combo_tuple)
        return self.retrieve_path_flat(combo)

    def get_unique_paths_with_conditions(self):
        seen = set()      # θα κρατάει τα path_key που έχουμε ήδη επεξεργαστεί
        unique = []       # τελική λίστα με dicts {'condition':…, 'path':…}

        for combo in self.define_possible_paths():
            # μετατρέπω το combo σε tuple ώστε να το δώσω στο cached wrapper
            combo_key = tuple(combo)  
            # παίρνω μονο φορά το path
            path = self._cached_retrieve_path_flat(combo_key)

            # χτίζω το key για uniqueness
            path_key = tuple(
                (step['Description'], step.get('ConditionValue'))
                for step in path
            )
            if path_key in seen:
                continue

            seen.add(path_key)
            unique.append({
                'condition': combo,
                'path': path
            })

        return unique