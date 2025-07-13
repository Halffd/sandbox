#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

// Define a structure for nodes in the adjacency list
typedef struct Node {
    char vertex;
    struct Node* next;
} Node;

// Define a structure for the graph
typedef struct Graph {
    int numVertices;
    Node** adjLists;
} Graph;

// Create a new node
Node* createNode(char vertex) {
    Node* newNode = (Node*)malloc(sizeof(Node));
    newNode->vertex = vertex;
    newNode->next = NULL;
    return newNode;
}

// Create a graph with given number of vertices
Graph* createGraph(int vertices) {
    Graph* graph = (Graph*)malloc(sizeof(Graph));
    graph->numVertices = vertices;
    
    // Create an array of adjacency lists
    graph->adjLists = (Node**)malloc(vertices * sizeof(Node*));
    
    // Initialize each adjacency list as empty
    for (int i = 0; i < vertices; i++) {
        graph->adjLists[i] = NULL;
    }
    
    return graph;
}

// Add an edge to the graph
void addEdge(Graph* graph, int src, int dest) {
    // Add edge from src to dest
    Node* newNode = createNode('A' + dest);
    newNode->next = graph->adjLists[src];
    graph->adjLists[src] = newNode;
}

// Print the graph
void printGraph(Graph* graph) {
    for (int i = 0; i < graph->numVertices; i++) {
        printf("%c -> ", 'A' + i);
        Node* temp = graph->adjLists[i];
        while (temp) {
            printf("%c -> ", temp->vertex);
            temp = temp->next;
        }
        printf("NULL\n");
    }
}

// DFS traversal from a given vertex
void DFSUtil(Graph* graph, int vertex, bool* visited) {
    // Mark the current vertex as visited
    visited[vertex] = true;
    printf("%c ", 'A' + vertex);
    
    // Recur for all adjacent vertices
    Node* temp = graph->adjLists[vertex];
    while (temp) {
        int adjVertex = temp->vertex - 'A';
        if (!visited[adjVertex]) {
            DFSUtil(graph, adjVertex, visited);
        }
        temp = temp->next;
    }
}

// DFS traversal starting from a specified vertex
void DFS(Graph* graph, int startVertex) {
    // Create a visited array
    bool* visited = (bool*)malloc(graph->numVertices * sizeof(bool));
    for (int i = 0; i < graph->numVertices; i++) {
        visited[i] = false;
    }
    
    printf("DFS traversal starting from vertex %c: ", 'A' + startVertex);
    DFSUtil(graph, startVertex, visited);
    printf("\n");
    
    free(visited);
}

// Find a path from source to destination using DFS
bool findPathDFSUtil(Graph* graph, int src, int dest, bool* visited, int* path, int* pathLen) {
    // Add current vertex to path
    path[*pathLen] = src;
    (*pathLen)++;
    
    // If destination is found, return true
    if (src == dest) {
        return true;
    }
    
    // Mark current vertex as visited
    visited[src] = true;
    
    // Recur for all adjacent vertices
    Node* temp = graph->adjLists[src];
    while (temp) {
        int adjVertex = temp->vertex - 'A';
        if (!visited[adjVertex]) {
            if (findPathDFSUtil(graph, adjVertex, dest, visited, path, pathLen)) {
                return true;
            }
        }
        temp = temp->next;
    }
    
    // If no path is found, remove the current vertex from path
    (*pathLen)--;
    return false;
}

// Find and print a path from source to destination
void findPath(Graph* graph, int src, int dest) {
    // Create a visited array
    bool* visited = (bool*)malloc(graph->numVertices * sizeof(bool));
    for (int i = 0; i < graph->numVertices; i++) {
        visited[i] = false;
    }
    
    // Create an array to store the path
    int* path = (int*)malloc(graph->numVertices * sizeof(int));
    int pathLen = 0;
    
    printf("Finding path from %c to %c: ", 'A' + src, 'A' + dest);
    
    if (findPathDFSUtil(graph, src, dest, visited, path, &pathLen)) {
        printf("Path found: ");
        for (int i = 0; i < pathLen; i++) {
            printf("%c", 'A' + path[i]);
            if (i < pathLen - 1) {
                printf(" -> ");
            }
        }
        printf("\n");
    } else {
        printf("No path exists from %c to %c\n", 'A' + src, 'A' + dest);
    }
    
    free(visited);
    free(path);
}

// Free the graph memory
void freeGraph(Graph* graph) {
    for (int i = 0; i < graph->numVertices; i++) {
        Node* current = graph->adjLists[i];
        while (current) {
            Node* temp = current;
            current = current->next;
            free(temp);
        }
    }
    free(graph->adjLists);
    free(graph);
}

int main() {
    // Create a graph with 5 vertices (A, B, C, D, E)
    Graph* graph = createGraph(5);
    
    // Add edges according to the diagram
    // A has edge to B
    addEdge(graph, 0, 1);
    
    // B has edges to C
    addEdge(graph, 1, 2);
    
    // C has edges to D and E
    addEdge(graph, 2, 3);
    addEdge(graph, 2, 4);
    
    // D has no outgoing edges (empty list)
    
    // E has edges to A and C
    addEdge(graph, 4, 0);
    addEdge(graph, 4, 2);
    
    // Print the adjacency list representation of the graph
    printf("Graph Adjacency List:\n");
    printGraph(graph);
    
    // Perform DFS traversal starting from vertex A
    DFS(graph, 0);
    
    // Find paths between various vertices
    findPath(graph, 0, 3); // A to D
    findPath(graph, 4, 3); // E to D
    findPath(graph, 3, 0); // D to A (should show no path)
    
    // Interactive path finding
    char startChar, endChar;
    printf("\nEnter starting vertex (A-E): ");
    scanf(" %c", &startChar);
    printf("Enter destination vertex (A-E): ");
    scanf(" %c", &endChar);
    
    if (startChar >= 'A' && startChar <= 'E' && endChar >= 'A' && endChar <= 'E') {
        int start = startChar - 'A';
        int end = endChar - 'A';
        findPath(graph, start, end);
    } else {
        printf("Invalid vertices. Vertices should be between A and E.\n");
    }
    
    // Free memory
    freeGraph(graph);
    
    return 0;
}