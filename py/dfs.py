def dfs_recursive_with_details(graph, node, visited=None, depth=0, visit_order=None):
    """
    Recursive DFS implementation with detailed printing
    
    Args:
        graph: A dictionary where keys are nodes and values are lists of adjacent nodes
        node: Current node being visited
        visited: Set of visited nodes
        depth: Current recursion depth for pretty printing
        visit_order: List to track the order of nodes visited
    
    Returns:
        List of nodes in the order they were visited
    """
    # Initialize on first call
    if visited is None:
        visited = set()
    if visit_order is None:
        visit_order = []
    
    # Print entry into this node
    indent = "  " * depth
    print(f"{indent}Entering node: {node}")
    
    # Mark the current node as visited and add to visit order
    visited.add(node)
    visit_order.append(node)
    print(f"{indent}Marked {node} as visited")
    print(f"{indent}Current visit order: {visit_order}")
    
    # Explore each neighbor
    print(f"{indent}Neighbors of {node}: {graph[node]}")
    for neighbor in graph[node]:
        if neighbor not in visited:
            print(f"{indent}Going deeper: {node} -> {neighbor}")
            dfs_recursive_with_details(graph, neighbor, visited, depth + 1, visit_order)
        else:
            print(f"{indent}Skipping {neighbor} (already visited)")
    
    print(f"{indent}Finished exploring {node}")
    return visit_order

def dfs_iterative_with_details(graph, start_node):
    """
    Iterative DFS implementation with detailed printing
    
    Args:
        graph: A dictionary where keys are nodes and values are lists of adjacent nodes
        start_node: The node to start the search from
    
    Returns:
        List of nodes in the order they were visited
    """
    stack = [start_node]
    visited = set()
    visit_order = []
    
    print(f"Starting iterative DFS from node {start_node}")
    print(f"Initial stack: {stack}")
    print(f"Initial visited: {visited}\n")
    
    step = 1
    while stack:
        # Get the top element from the stack
        current = stack.pop()
        print(f"Step {step}:")
        print(f"  Popped: {current}")
        
        # If we haven't visited this node yet
        if current not in visited:
            print(f"  Marking {current} as visited")
            visited.add(current)
            visit_order.append(current)
            print(f"  Current visit order: {visit_order}")
            
            # Add neighbors to stack (in reverse order to match recursive DFS)
            print(f"  Examining neighbors of {current}: {graph[current]}")
            # Adding in reverse order so that the first neighbor will be popped first
            for neighbor in reversed(graph[current]):
                if neighbor not in visited:
                    print(f"    Pushed {neighbor} to stack (not visited)")
                    stack.append(neighbor)
                else:
                    print(f"    Skipped pushing {neighbor} (already visited)")
        else:
            print(f"  Node {current} already visited, skipping")
            
        print(f"  Updated stack: {stack}")
        print(f"  Updated visited: {visited}\n")
        step += 1
    
    print(f"DFS complete. Final visit order: {visit_order}")
    return visit_order

# Example usage:
if __name__ == "__main__":
    # Sample graph represented as an adjacency list
    graph = {
        'A': ['B', 'C'],
        'B': ['A', 'D', 'E'],
        'C': ['A', 'F'],
        'D': ['B'],
        'E': ['B', 'F'],
        'F': ['C', 'E']
    }
    
    print("===== RECURSIVE DFS =====")
    result_recursive = dfs_recursive_with_details(graph, 'A')
    print("\nFinal result (recursive):", result_recursive)
    
    print("\n\n===== ITERATIVE DFS =====")
    result_iterative = dfs_iterative_with_details(graph, 'A')
    print("\nFinal result (iterative):", result_iterative)
