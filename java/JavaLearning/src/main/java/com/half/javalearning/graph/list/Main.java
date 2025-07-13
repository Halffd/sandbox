package com.half.javalearning.graph.list;

public class Main {

    public static void main(String[] args) {

        // Adjacency List = An array/arraylist of linkedlists.
        //			          Each LinkedList has a unique node at the head.
        //			          All adjacent neighbors to that node are added to that node's linkedlist

        //				  runtime complexity to check an Edge: O(v)
        //				  space complexity: O(v + e)

        Graph graph = new Graph();

        graph.addNode(new Node("A"));
        graph.addNode(new Node("B"));
        graph.addNode(new Node("C"));
        graph.addNode(new Node("D"));
        graph.addNode(new Node("E"));

        graph.addEdge(0, 1);
        graph.addEdge(1, 2);
        graph.addEdge(1, 4);
        graph.addEdge(2, 3);
        graph.addEdge(2, 4);
        graph.addEdge(4, 0);
        graph.addEdge(4, 2);

        graph.print();

        System.out.println(graph.checkEdge(4, 2));
        System.out.println("\nDFS Traversal starting from A:");
        graph.dfs(0);
        System.out.println("\nDFS Traversal starting from C:");
        graph.dfs(2);
        Graph graph2 = new Graph();
        graph2.addNode(new Node("A"));
        graph2.addNode(new Node("B"));
        graph2.addNode(new Node("C"));
        graph2.addNode(new Node("D"));
        graph2.addNode(new Node("E"));

        graph2.addEdge(0, 1);
        graph2.addEdge(1, 2);
        graph2.addEdge(2, 4);
        graph2.addEdge(4, 3);
        graph2.addEdge(3, 4);
        graph2.addEdge(4, 0);
        graph2.addEdge(4, 2);

        graph2.print();

        graph2.breadthFirstSearch(0);
        graph2.breadthFirstSearch(2);
    }
}
