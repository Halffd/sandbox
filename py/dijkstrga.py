def dijkstra_shortest_path(graph, a, z):
    """
    Find the shortest path between nodes a and z in a weighted graph.
    
    Args:
        graph: A dictionary of dictionaries representing the weighted graph
               where graph[u][v] is the weight of edge (u,v)
        a: Starting node
        z: Target node
    
    Returns:
        The length of the shortest path from a to z
    """
    # Initialize distances
    L = {}
    L[a] = 0
    
    # Set all other nodes to infinity distance
    for x in graph:
        if x != a:
            L[x] = float('inf')
   
    # Set of nodes whose minimum distance hasn't been calculated yet
    T = set(graph.keys())
    
    # While z is still in T
    while z in T:
        # Find node v in T with minimum L[v]
        v = min(T, key=lambda x: L[x])
        
        # Remove v from T
        T.remove(v)
        
        # Update distances for neighbors of v that are still in T
        for x in T:
            if x in graph[v]:  # If x is a neighbor of v
                L[x] = min(L[x], L[v] + graph[v][x])
    
    return L[z]
graph = {
    'a': {'b': 1, 'c': 4},
    'b': {'c': 2, 'd': 5},
    'c': {'d': 1},
    'd': {'z': 3},
    'z': {}
}

shortest_path_length = dijkstra_shortest_path(graph, 'a', 'z')
print(f"The shortest path from a to z has length: {shortest_path_length}")
