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

// Function declarations
Node* createNode(char vertex);
Graph* createGraph(int vertices);
void addEdge(Graph* graph, int src, int dest);
void printGraph(Graph* graph);
void findLongestPathDFS(Graph* graph, int src, int dest, bool* visited, 
                       int* currentPath, int* currentPathLen, 
                       int* longestPath, int* longestPathLen);
void findLongestPath(Graph* graph, int src, int dest);
void findLongestPathInGraph(Graph* graph);
void freeGraph(Graph* graph);

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

// Helper function to find the longest path using DFS
void findLongestPathDFS(Graph* graph, int src, int dest, bool* visited, 
                       int* currentPath, int* currentPathLen, 
                       int* longestPath, int* longestPathLen) {
    // Add current vertex to path
    currentPath[*currentPathLen] = src;
    (*currentPathLen)++;
    
    // Mark current vertex as visited
    visited[src] = true;
    
    // If destination is reached, check if current path is longer than longest path
    if (src == dest) {
        if (*currentPathLen > *longestPathLen) {
            *longestPathLen = *currentPathLen;
            for (int i = 0; i < *currentPathLen; i++) {
                longestPath[i] = currentPath[i];
            }
        }
    } else {
        // Explore all adjacent vertices
        Node* temp = graph->adjLists[src];
        while (temp) {
            int adjVertex = temp->vertex - 'A';
            if (!visited[adjVertex]) {
                findLongestPathDFS(graph, adjVertex, dest, visited, currentPath, 
                                 currentPathLen, longestPath, longestPathLen);
            }
            temp = temp->next;
        }
    }
    
    // Backtrack: remove current vertex from path and mark it as unvisited
    (*currentPathLen)--;
    visited[src] = false;
}

// Function to find the longest path from source to destination
void findLongestPath(Graph* graph, int src, int dest) {
    // Arrays to track the current path and the longest path found
    int* currentPath = (int*)malloc(graph->numVertices * sizeof(int));
    int* longestPath = (int*)malloc(graph->numVertices * sizeof(int));
    int currentPathLen = 0;
    int longestPathLen = 0;
    
    // Visited array to avoid cycles during traversal
    bool* visited = (bool*)malloc(graph->numVertices * sizeof(bool));
    for (int i = 0; i < graph->numVertices; i++) {
        visited[i] = false;
    }
    
    // Use a helper function to perform DFS and find the longest path
    findLongestPathDFS(graph, src, dest, visited, currentPath, &currentPathLen, 
                      longestPath, &longestPathLen);
    
    // Print the longest path found
    if (longestPathLen > 0) {
        printf("Longest path from %c to %c (length %d): ", 'A' + src, 'A' + dest, longestPathLen - 1);
        for (int i = 0; i < longestPathLen; i++) {
            printf("%c", 'A' + longestPath[i]);
            if (i < longestPathLen - 1) {
                printf(" -> ");
            }
        }
        printf("\n");
    } else {
        printf("No path exists from %c to %c\n", 'A' + src, 'A' + dest);
    }
    
    free(currentPath);
    free(longestPath);
    free(visited);
}

// Function to find the longest path in the entire graph
void findLongestPathInGraph(Graph* graph) {
    int* overallLongestPath = (int*)malloc(graph->numVertices * sizeof(int));
    int overallLongestPathLen = 0;
    int srcVertex = 0, destVertex = 0;
    
    // Try every source-destination pair
    for (int src = 0; src < graph->numVertices; src++) {
        for (int dest = 0; dest < graph->numVertices; dest++) {
            if (src != dest) {
                int* currentPath = (int*)malloc(graph->numVertices * sizeof(int));
                int* longestPath = (int*)malloc(graph->numVertices * sizeof(int));
                int currentPathLen = 0;
                int longestPathLen = 0;
                
                bool* visited = (bool*)malloc(graph->numVertices * sizeof(bool));
                for (int i = 0; i < graph->numVertices; i++) {
                    visited[i] = false;
                }
                
                findLongestPathDFS(graph, src, dest, visited, currentPath, &currentPathLen, 
                                  longestPath, &longestPathLen);
                
                if (longestPathLen > 0 && longestPathLen > overallLongestPathLen) {
                    overallLongestPathLen = longestPathLen;
                    for (int i = 0; i < longestPathLen; i++) {
                        overallLongestPath[i] = longestPath[i];
                    }
                    srcVertex = src;
                    destVertex = dest;
                }
                
                free(currentPath);
                free(longestPath);
                free(visited);
            }
        }
    }
    
    // Print the overall longest path
    if (overallLongestPathLen > 0) {
        printf("Overall longest path in the graph (from %c to %c, length %d): ", 
               'A' + srcVertex, 'A' + destVertex, overallLongestPathLen - 1);
        for (int i = 0; i < overallLongestPathLen; i++) {
            printf("%c", 'A' + overallLongestPath[i]);
            if (i < overallLongestPathLen - 1) {
                printf(" -> ");
            }
        }
        printf("\n");
    } else {
        printf("No paths found in the graph.\n");
    }
    
    free(overallLongestPath);
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
    
    // Find the longest path between specific vertices
    printf("\nFinding longest paths between specific vertices:\n");
    findLongestPath(graph, 0, 3); // A to D
    findLongestPath(graph, 4, 3); // E to D
    
    // Find the longest path in the entire graph
    printf("\nFinding the longest path in the entire graph:\n");
    findLongestPathInGraph(graph);
    
    // Free memory
    freeGraph(graph);
    
    return 0;
}