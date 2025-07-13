package com.half.javalearning.graph.list;
import java.util.*;
public class Graph {

    ArrayList<LinkedList<Node>> alist;

    Graph(){
        alist = new ArrayList<>();
    }

    public void addNode(Node node) {
        LinkedList<Node> currentList = new LinkedList<>();
        currentList.add(node);
        alist.add(currentList);
    }
    public void addEdge(int src, int dst) {
        LinkedList<Node> currentList = alist.get(src);
        System.out.println(currentList);
        Node dstNode = alist.get(dst).get(0);
        System.out.println(dstNode);
        currentList.add(dstNode);
    }
    public boolean checkEdge(int src, int dst) {
        LinkedList<Node> currentList = alist.get(src);
        Node dstNode = alist.get(dst).get(0);
        System.out.println(dstNode);
        return currentList.contains(dstNode);
    }
    public void print() {
        for(LinkedList<Node> currentList : alist) {
            for(Node node : currentList) {
                System.out.print(node.data + " -> ");
            }
            System.out.println();
        }
    }
    public void dfs(int start) {
        boolean[] visited = new boolean[alist.size()];
        dfsHelper(start, visited);
        System.out.println(); // Add a newline at the end
    }

    private void dfsHelper(int current, boolean[] visited) {
        // Mark current node as visited and print it
        visited[current] = true;
        Node currentNode = alist.get(current).get(0); // Get the current node
        System.out.print(currentNode + " -> ");

        // Get adjacency list for current node (skip the first element which is the node itself)
        LinkedList<Node> neighbors = alist.get(current);

        // For each adjacent vertex, if not visited, recursively call DFS
        for (int i = 1; i < neighbors.size(); i++) {
            Node adjacentNode = neighbors.get(i);

            // Find the index of this adjacent node in our main list
            int adjacentIndex = -1;
            for (int j = 0; j < alist.size(); j++) {
                if (alist.get(j).get(0).equals(adjacentNode)) {
                    adjacentIndex = j;
                    break;
                }
            }

            // If found and not visited, visit it
            if (adjacentIndex != -1 && !visited[adjacentIndex]) {
                dfsHelper(adjacentIndex, visited);
            }
        }
    }


    public void breadthFirstSearch(int start) {
        Queue<Integer> queue = new LinkedList<>();
        boolean[] visited = new boolean[alist.size()];

        queue.offer(start);
        visited[start] = true;

        while (!queue.isEmpty()) {
            int current = queue.poll();
            Node currentNode = alist.get(current).get(0);
            System.out.println(currentNode.data + " = visited");

            // Get adjacency list for current node (skip first element which is the node itself)
            LinkedList<Node> neighbors = alist.get(current);

            // Process each neighbor
            for (int i = 1; i < neighbors.size(); i++) {
                Node adjacentNode = neighbors.get(i);

                // Find the index of this adjacent node in our main list
                int adjacentIndex = -1;
                for (int j = 0; j < alist.size(); j++) {
                    if (alist.get(j).get(0).equals(adjacentNode)) {
                        adjacentIndex = j;
                        break;
                    }
                }

                // If found and not visited, add to queue
                if (adjacentIndex != -1 && !visited[adjacentIndex]) {
                    queue.offer(adjacentIndex);
                    visited[adjacentIndex] = true;
                }
            }
        }
    }
}