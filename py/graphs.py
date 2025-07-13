import networkx as nx
import matplotlib.pyplot as plt
from itertools import permutations

# Function to create and display a graph

def display_graph(G, title, pos=None, path=None):
    plt.figure(figsize=(8, 6))
    if pos is None:
        pos = nx.spring_layout(G, seed=42)

    # default styles
    node_colors = ['skyblue'] * len(G.nodes())
    edge_colors = ['gray'] * len(G.edges())
    width = [1] * len(G.edges())

    # if a path is provided, highlight
    if path:
        # highlight nodes in path
        node_index = {node: i for i, node in enumerate(G.nodes())}
        for node in path:
            idx = list(G.nodes()).index(node)
            node_colors[idx] = 'orange'
        # highlight edges in path
        path_edges = list(zip(path, path[1:]))
        for i, edge in enumerate(G.edges()):
            if edge in path_edges or (not G.is_directed() and edge[::-1] in path_edges):
                edge_colors[i] = 'red'
                width[i] = 2.5

    nx.draw(G, pos,
            with_labels=True,
            node_color=node_colors,
            node_size=800,
            font_size=10,
            font_weight='bold',
            edge_color=edge_colors,
            width=width,
            arrowsize=20)
    plt.title(title)
    plt.show()
    return pos

# Hamiltonian path finder (brute force)

def find_hamiltonian_paths(G):
    nodes = list(G.nodes())
    paths = []
    for perm in permutations(nodes):
        valid = True
        for u, v in zip(perm, perm[1:]):
            if not G.has_edge(u, v):
                valid = False
                break
        if valid:
            paths.append(perm)
    return paths

# Analyze graph: connectivity, Hamiltonian paths

def analyze_graph(G, title):
    print(f"Analyzing {title}")
    # Check connectivity
    if G.is_directed():
        connected = nx.is_strongly_connected(G)
        print(f"Strongly connected: {connected}")
    else:
        connected = nx.is_connected(G)
        print(f"Connected: {connected}")

    # Find Hamiltonian paths
    h_paths = find_hamiltonian_paths(G)
    print(f"Found {len(h_paths)} Hamiltonian path(s)")
    # Optionally list first few paths
    for i, p in enumerate(h_paths[:5], 1):
        print(f"  Path {i}: {p}")
    if len(h_paths) > 5:
        print("  ...")

    # Display each path graphically
    pos = nx.spring_layout(G, seed=42)
    for i, p in enumerate(h_paths, 1):
        display_graph(G, f"{title} -- Hamiltonian Path {i}", pos, path=p)

    return connected, h_paths

# Example usage

if __name__ == "__main__":
    # Original tournament K6 example
    teams = ["11A", "11B", "21A", "22B", "31A", "31B"]
    G_teams = nx.complete_graph(teams)
    display_graph(G_teams, "Tournament Complete Graph K6")
    analyze_graph(G_teams, "K6 Tournament")

    # Played vs remaining edges
    G_played = nx.Graph()
    G_played.add_nodes_from(teams)
    played_matches = [("11A", "11B"), ("11A", "21A"), ("11B", "21A"),
                      ("22B", "31A"), ("31A", "31B")]
    G_played.add_edges_from(played_matches)
    display_graph(G_played, "Played Games")
    analyze_graph(G_played, "Played Games")

    # Other graph types
    G_null = nx.Graph()
    G_null.add_nodes_from(range(1, 7))
    display_graph(G_null, "Null Graph with 6 vertices")
    analyze_graph(G_null, "Null Graph")

    G_regular = nx.random_regular_graph(3, 6)
    display_graph(G_regular, "3-Regular Graph with 6 vertices")
    analyze_graph(G_regular, "3-Regular Graph")

    G_cycle = nx.cycle_graph(6)
    display_graph(G_cycle, "Cycle Graph with 6 vertices")
    analyze_graph(G_cycle, "Cycle Graph")

    G_dir = nx.DiGraph()
    G_dir.add_nodes_from(['a', 'b', 'c', 'd', 'e'])
    G_dir.add_edges_from([('a','b'),('b','c'),('c','d'),('d','a'),('a','e'),('e','c')])
    display_graph(G_dir, "Directed Graph")
    analyze_graph(G_dir, "Directed Graph")
