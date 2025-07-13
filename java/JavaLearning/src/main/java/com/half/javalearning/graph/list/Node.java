package com.half.javalearning.graph.list;
import java.util.Objects;

public class Node {
    String data;

    public Node(String data) {
        this.data = data;
    }

    @Override
    public String toString() {
        return data;
    }
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Node node)) return false;
        return Objects.equals(data, node.data);
    }

    @Override
    public int hashCode() {
        return Objects.hash(data);
    }
}
