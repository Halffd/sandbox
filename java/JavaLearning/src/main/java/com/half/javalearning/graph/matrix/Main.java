package com.half.javalearning.graph.matrix;

public class Main {

    public static void main(String[] args) {

        // Adjacency Matrix = An array to store 1's/0's to represent edges
        //				       # of rows =    # of unique nodes
        //				       # of columns = # of unique nodes

        //					  runtime complexity to check an Edge: O(1)
        //					  space complexity: O(v^2)

        Graph graph = new Graph(7);

        graph.addNode(new Node('a'));
        graph.addNode(new Node('b'));
        graph.addNode(new Node('c'));
        graph.addNode(new Node('d'));
        graph.addNode(new Node('e'));
        graph.addNode(new Node('f'));
        graph.addNode(new Node('g'));

        graph.addEdge(0, 1);
        graph.addEdge(1, 2);
        graph.addEdge(1, 4); // I forgot this line in the video
        graph.addEdge(2, 3);
        graph.addEdge(2, 4);
        graph.addEdge(4, 0);
        graph.addEdge(4, 2);
        graph.addEdge(2, 6);
        graph.addEdge(5, 6);
        graph.addEdge(5, 1);


        graph.print();

        System.out.println(graph.checkEdge(3, 4));
    }
}
