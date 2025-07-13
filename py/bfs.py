from collections import deque

def bfs(graph, start_node):
    """
    Perform a breadth-first search on a graph.
    
    Args:
        graph: A dictionary where keys are nodes and values are lists of adjacent nodes
        start_node: The node to start the search from
    
    Returns:
        A list of nodes in the order they were visited
    """
    # Initialize visited set and queue
    visited = set()
    queue = deque([start_node])
    visited.add(start_node)
    
    # List to store the order of nodes visited
    visit_order = []
    
    # Process nodes in the queue
    while queue:
        # Get the next node from the front of the queue
        current = queue.popleft()
        visit_order.append(current)
        
        # Add all unvisited neighbors to the queue
        for neighbor in graph[current]:
            if neighbor not in visited:
                visited.add(neighbor)
                queue.append(neighbor)
    
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
    
    # Perform BFS starting from node 'A'
    result = bfs(graph, 'A')
    print("BFS traversal order:", result)
