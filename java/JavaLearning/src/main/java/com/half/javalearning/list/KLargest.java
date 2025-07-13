package com.half.javalearning.list;
import java.util.*;
import java.util.stream.Collectors;
import static java.util.function.Function.identity;
import static java.util.stream.Collectors.*;

public class KLargest {
    static List<String> getMostCommonWordsIn(List<String> list, int limit) {
        Map<String, Long> counts = list.stream()
                .collect(groupingBy(identity(), counting()));

        // Use PriorityQueue to maintain top K without full sort
        PriorityQueue<Map.Entry<String, Long>> topK = new PriorityQueue<>(
                limit, Map.Entry.comparingByValue()
        );

        for (Map.Entry<String, Long> entry : counts.entrySet()) {
            if (topK.size() < limit) {
                topK.offer(entry);
            } else if (entry.getValue() > topK.peek().getValue()) {
                topK.poll();
                topK.offer(entry);
            }
        }

        return topK.stream()
                .sorted(Map.Entry.<String, Long>comparingByValue().reversed())
                .map(Map.Entry::getKey)
                .collect(toList());
    }

    public static void main(String[] args) {
        List<String> list = List.of("a", "a", "b", "c", "c", "c");
        System.out.println(getMostCommonWordsIn(list, 2)); // [c, a]
    }
}