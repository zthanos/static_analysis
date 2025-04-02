from logger import logger
import logging
from tree_node import TreeNode
from itertools import product

class MyTree:
    def __init__(self):
        self.root=None
    def add_root(self, data, kindof=None):
        if isinstance(data, TreeNode):
            self.root = data
            return self.root
        if self.root is None:
            self.root = TreeNode(data, kindof)
            return self.root
        # else:
        #     return self.root.add_child(data)
        
    def add_child(self, data):
        return self.root.add_child(data)
    
    def print_tree(self):
        tree_data = self.root.print_tree()
        for node in tree_data:
            logger.info(node)
    
    def get_node_by_name(self, name):
        # locate a node by name in all nodes starting from the root
        def _get_node_by_name(node, name):
            if node.methodName == name:
                return node
            for child in node.children:
                result = _get_node_by_name(child, name)
                if result:
                    return result
            return None
        
        return _get_node_by_name(self.root, name)


    def get_condition_paths(self, node, path):
        search_path = "True Path" if path else "False Path"
        condition_path = [child for child in node.children if child.methodName == search_path]
        if not condition_path:
            return []
        branch_true =[]
        branch_true.append(f'{node.methodName}({path})')
        for st in condition_path[0].children:
            if st.type=="StatementType.CONDITION":
                branch_true.extend(self.get_path(st, search_path))
            else:
                branch_true.append(st.methodName)
            # logger.info(st.methodName)        
        return branch_true
    
    def get_path(self, node, path=True):
        branch =[]
        if node.type=="StatementType.CONDITION":
            branch.extend(self.get_condition_paths(node, path))
        else:
            branch.append(node.methodName)
        return branch
                    
    # def define_posible_paths(self, level=0, node = None):
    #     paths = []

    #     children = node.children if node else self.root.children
    #     for p in children:
    #         if p.type=="StatementType.CONDITION":
    #             condition = p.children[0]
    #             paths.append(True)
    #             for c in condition.children:
    #                 paths.append(self.define_posible_paths(level+1, c))
    #     return paths
    
    def define_possible_paths(self):
        pos_path = self.root.get_flat_conditions_structure()
        
        return self.generate_all_combinations(pos_path)

    def retrieve_path(self, condition_branching, level=0, node=None, condition_index=0):
        path = []
        
        if node is None:
            node = self.root
        
        # Add current node's method name
        path.append(node.methodName)
        
        # Base case: leaf node
        if not node.children:
            return path
        
        current_condition_index = condition_index
        
        for child in node.children:
            if getattr(child, 'type', None) == "StatementType.CONDITION":
                # Check if we have branching info for this condition
                if level < len(condition_branching) and current_condition_index < len(condition_branching[level]):
                    decision = condition_branching[level][current_condition_index]
                    current_condition_index += 1
                    
                    path.append(f"{child.methodName}({decision})")
                    
                    # Find matching path child (True/False Path)
                    path_found = False
                    for grandchild in child.children:
                        if ((decision and "True Path" in grandchild.methodName) or
                            (not decision and "False Path" in grandchild.methodName)):
                            path.extend(self.retrieve_path(
                                condition_branching,
                                level + 1,  # Increment level for conditions
                                grandchild,
                                0  # Reset condition index for new level
                            ))
                            path_found = True
                            break
                    
                    if not path_found and child.children:
                        # If no explicit True/False path, take first child
                        path.extend(self.retrieve_path(
                            condition_branching,
                            level + 1,
                            child.children[0],
                            0
                        ))
                else:
                    # If no branching info, add condition and process all children
                    path.append(child.methodName)
                    for grandchild in child.children:
                        path.extend(self.retrieve_path(
                            condition_branching,
                            level + 1,
                            grandchild,
                            0
                        ))
            else:
                # For non-condition nodes, process all children at same level
                path.append(child.methodName)
                for grandchild in child.children:
                    path.extend(self.retrieve_path(
                        condition_branching,
                        level,  # Maintain same level
                        grandchild,
                        current_condition_index  # Maintain condition index
                    ))
        
        return path

    def print_paths(self, condition_branching, node  = None):
        if node is None:
            node = self.root
        paths = []
        if not condition_branching:
            condition_branching=[]

        np = []
        for index, statement in enumerate(node.children):
            if statement.type=="StatementType.CONDITION":
                if index > len(condition_branching) -1:
                    condition_branching.append(True)
                else:
                    condition_branching[index]=not condition_branching[index]
                condition_path = condition_branching[index]
                np.extend(self.get_condition_paths(condition_path, statement))
            else:
                np.append(statement.methodName)
        for np in paths:
                logger.info("  ".join(p))
        paths.append(np)
        self.print_paths(node, 0, condition_branching)
        for p in paths:
            logger.info("  ".join(p))
        a=1

    def generate_all_combinations(self, condition_branching):
        # Convert the input to a mutable structure (list of lists)
        original = [list(level) for level in condition_branching]
        combinations = []
        
        def backtrack(current, level, index):
            # Add the current state to combinations
            combinations.append([list(level) for level in current])
            
            # Work backwards through levels
            for l in range(len(current)-1, level-1, -1):
                # Work backwards through conditions in each level
                for i in range(len(current[l])-1, index-1, -1):
                    if current[l][i]:  # If it's True, flip it to False
                        # Create new version with this condition flipped
                        new_version = [list(lev) for lev in current]
                        new_version[l][i] = False
                        
                        # Continue backtracking from this position
                        backtrack(new_version, l, i)
        
        # Start with the original all-True version
        backtrack(original, 0, 0)
        
        # Remove duplicates while preserving order
        seen = set()
        unique_combinations = []
        for combo in combinations:
            tuple_version = tuple(tuple(level) for level in combo)
            if tuple_version not in seen:
                seen.add(tuple_version)
                unique_combinations.append(combo)
        
        return unique_combinations
    
    
    
    def get_unique_paths(self, all_combinations):
        seen_paths = set()
        unique_paths = []
        
        for combo in all_combinations:
            # Convert the path to a string for easy comparison
            path_str = " -> ".join(combo)
            
            # Only add if we haven't seen this path before
            if path_str not in seen_paths:
                seen_paths.add(path_str)
                unique_paths.append(combo)
        
        return unique_paths    
    
    def get_unique_paths_with_conditions(self):
        # all_combinations = self.define_possible_paths()
        # logger.info(all_combinations)
        path_map = {}
        
        a = self.root.get_flat_conditions_structure()
        combs = self.generate_all_flat_combinations(a)
        
        # for flat_combo in combs:
        #     path = self.retrieve_path_flat(flat_combo)
        #     logger.info(" -> ".join(path))
        
        
        for combo in combs:
            path = self.retrieve_path_flat(combo)
            # path = self.retrieve_path(combo)
            path_str = " -> ".join(path)
            logger.info(path_str)
        #   # Only store the first condition combination that leads to this path
            if path_str not in path_map:
                path_map[path_str] = {
                    'condition': combo,
                    'path': path
                }
        
        # Extract the unique paths with their representative conditions
        unique_results = list(path_map.values())
        
        # Sort by some meaningful order (e.g., most True values first)
        # unique_results.sort(key=lambda x: sum(sum(level) for level in x['condition']), reverse=True)
        
        return unique_results    
    
    def retrieve_path_flat(self, condition_sequence, node=None, condition_index=0):
        path = []

        if node is None:
            node = self.root

        # Προσθέτουμε το τρέχον node
        path.append(node.methodName)

        for child in node.children:
            if getattr(child, 'type', None) == "StatementType.CONDITION":
                if condition_index < len(condition_sequence):
                    decision = condition_sequence[condition_index]
                    condition_index += 1

                    path.append(f"{child.methodName}({decision})")

                    # Εύρεση True/False Path
                    for branch in child.children:
                        if decision and "True Path" in branch.methodName:
                            path.extend(self.retrieve_path_flat(condition_sequence, branch, condition_index))
                            break
                        elif not decision and "False Path" in branch.methodName:
                            path.extend(self.retrieve_path_flat(condition_sequence, branch, condition_index))
                            break
                else:
                    # Δεν υπάρχει άλλη πληροφορία από το sequence
                    path.append(child.methodName)
                    for branch in child.children:
                        path.extend(self.retrieve_path_flat(condition_sequence, branch, condition_index))
            else:
                # Κανονικό statement: επεξεργασία κανονική
                path.append(child.methodName)
                for grandchild in child.children:
                    path.extend(self.retrieve_path_flat(condition_sequence, grandchild, condition_index))

        return path

    def generate_all_flat_combinations(self, flat_condition_structure):
        # Δημιουργεί όλους τους πιθανούς συνδυασμούς True/False για τις θέσεις της flat λίστας
        return [list(combo) for combo in product([True, False], repeat=len(flat_condition_structure))]