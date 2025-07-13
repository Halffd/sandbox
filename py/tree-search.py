import time
import random
import matplotlib.pyplot as plt
import numpy as np
import sys

# Increase recursion limit for search, but use iterative insert for large trees
sys.setrecursionlimit(10000)

class Node:
    def __init__(self, key):
        self.key = key
        self.left = None
        self.right = None

def search(key, root, parent=None):
    """
    Search for a key in a binary search tree.
    
    Args:
        key: The key to search for
        root: The root node of the tree
        parent: The parent of the current node
        
    Returns:
        tuple: (node, parent, found)
            - node: The node containing the key or where search ended
            - parent: The parent of the found node
            - found: Boolean indicating if key was found
    """
    if root is None:
        return None, parent, False
    
    if root.key == key:
        return root, parent, True
    
    if key < root.key:
        if root.left is not None:
            return search(key, root.left, root)
        else:
            return root, parent, False
    else:  # key > root.key
        if root.right is not None:
            return search(key, root.right, root)
        else:
            return root, parent, False

def insert_iterative(key, root):
    """Insert a key into the BST using iteration instead of recursion"""
    new_node = Node(key)
    
    if root is None:
        return new_node
    
    current = root
    parent = None
    
    while current is not None:
        parent = current
        if key < current.key:
            current = current.left
        else:
            current = current.right
    
    # Now parent is the node where we should attach the new node
    if key < parent.key:
        parent.left = new_node
    else:
        parent.right = new_node
        
    return root

def build_tree(keys, use_iterative=False):
    """Build a BST from a list of keys"""
    root = None
    
    # Use iterative insert for large trees to avoid recursion limit
    insert_func = insert_iterative if use_iterative else insert_recursive
    
    for key in keys:
        if root is None:
            root = Node(key)
        else:
            insert_func(key, root)
    
    return root

def insert_recursive(key, root):
    """Insert a key into the BST recursively"""
    if key < root.key:
        if root.left is None:
            root.left = Node(key)
        else:
            insert_recursive(key, root.left)
    elif key > root.key:
        if root.right is None:
            root.right = Node(key)
        else:
            insert_recursive(key, root.right)
    # If key equals root.key, do nothing (no duplicates)

def inorder_traversal(root):
    """In-order traversal of the tree"""
    result = []
    if root:
        result.extend(inorder_traversal(root.left))
        result.append(root.key)
        result.extend(inorder_traversal(root.right))
    return result

# Example usage
print("Example 1: Basic Search")
tree = build_tree([40, 20, 60, 10, 30, 50, 70])
print("Tree (in-order):", inorder_traversal(tree))

# Test with various keys to verify parent tracking
test_keys = [30, 10, 60, 25]
for key in test_keys:
    node, parent, found = search(key, tree)
    print(f"Searching for {key}: Found = {found}")
    if found:
        print(f"  Node key: {node.key}, Parent key: {parent.key if parent else 'None (root)'}")
    else:
        print(f"  Would be inserted at parent: {node.key}")

print("\nExample 2: Time Complexity Analysis")
# Measure search times for different tree sizes
def measure_search_time(sizes, tree_type="random"):
    times = []
    
    for size in sizes:
        if tree_type == "random":
            keys = random.sample(range(1, size*10), size)
        elif tree_type == "sorted":
            keys = list(range(1, size+1))
        
        # Use iterative insertion for large trees
        tree = build_tree(keys, use_iterative=(size > 900))
        
        # Search for multiple random keys and take average
        trials = min(100, size)
        search_keys = random.sample(keys, trials) if size >= trials else keys
        
        start = time.time()
        for key in search_keys:
            search(key, tree)
        search_time = (time.time() - start) / len(search_keys)
        
        times.append(search_time)
        print(f"Tree size {size}: Average search time = {search_time:.8f} seconds")
    
    return times

# Generate data for time complexity graph
sizes = [10, 100, 1000, 2000, 5000]  # Reduced maximum size for faster execution
print("\nBalanced trees (random keys):")
random_times = measure_search_time(sizes, "random")
print("\nUnbalanced trees (sorted keys):")
sorted_times = measure_search_time(sizes, "sorted")

# Plot the results
plt.figure(figsize=(10, 6))
plt.plot(sizes, random_times, 'o-', label='Balanced tree (random keys)')
plt.plot(sizes, sorted_times, 's-', label='Unbalanced tree (sorted keys)')

# Add logarithmic reference line
log_scale = max(random_times[-1] / np.log2(sizes[-1]), 1e-6)
log_reference = [np.log2(n) * log_scale for n in sizes]
plt.plot(sizes, log_reference, '--', label='O(log n) reference')

# Add linear reference line
linear_scale = max(sorted_times[-1] / sizes[-1], 1e-7)
linear_reference = [n * linear_scale for n in sizes]
plt.plot(sizes, linear_reference, ':', label='O(n) reference')

plt.xlabel('Tree Size (number of nodes)')
plt.ylabel('Average Search Time (seconds)')
plt.title('BST Search Time Complexity')
plt.legend()
plt.grid(True)
plt.xscale('log')
plt.yscale('log')
plt.show()

print("\nTime Complexity Analysis:")
print("- For a balanced BST: O(log n)")
print("- For an unbalanced BST (worst case): O(n)")
print("- The graph demonstrates why balanced trees are critical for performance")
