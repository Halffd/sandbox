package com.half.javalearning.list;
import java.util.*;

public class Main {
    public static void main(String[] args) {
        System.out.println("=== COLLECTION TYPES DEMO ===\n");

        // SET - No duplicates, unordered (HashSet) vs ordered (TreeSet/LinkedHashSet)
        System.out.println("1. SET Examples:");

        HashSet<String> hashSet = new HashSet<>();
        hashSet.add("banana");
        hashSet.add("apple");
        hashSet.add("banana"); // duplicate - won't be added
        System.out.println("HashSet (unordered): " + hashSet);

        TreeSet<String> treeSet = new TreeSet<>();
        treeSet.add("banana");
        treeSet.add("apple");
        treeSet.add("cherry");
        System.out.println("TreeSet (sorted): " + treeSet);

        LinkedHashSet<String> linkedHashSet = new LinkedHashSet<>();
        linkedHashSet.add("banana");
        linkedHashSet.add("apple");
        linkedHashSet.add("cherry");
        System.out.println("LinkedHashSet (insertion order): " + linkedHashSet);

        // LIST - Allows duplicates, indexed access
        System.out.println("\n2. LIST Examples:");

        ArrayList<Integer> arrayList = new ArrayList<>();
        arrayList.add(10);
        arrayList.add(20);
        arrayList.add(10); // duplicate allowed
        System.out.println("ArrayList: " + arrayList);
        System.out.println("Element at index 1: " + arrayList.get(1));

        LinkedList<Integer> linkedList = new LinkedList<>();
        linkedList.add(100);
        linkedList.add(200);
        linkedList.addFirst(50); // efficient front insertion
        System.out.println("LinkedList: " + linkedList);

        // QUEUE - FIFO (First In, First Out)
        System.out.println("\n3. QUEUE Examples:");

        Queue<String> queue = new LinkedList<>();
        queue.offer("first");
        queue.offer("second");
        queue.offer("third");
        System.out.println("Queue: " + queue);
        System.out.println("Poll (remove): " + queue.poll()); // removes "first"
        System.out.println("After poll: " + queue);
        System.out.println("Peek (don't remove): " + queue.peek()); // shows "second"

        PriorityQueue<Integer> priorityQueue = new PriorityQueue<>();
        priorityQueue.offer(30);
        priorityQueue.offer(10);
        priorityQueue.offer(20);
        System.out.println("PriorityQueue: " + priorityQueue);
        System.out.println("Poll highest priority: " + priorityQueue.poll()); // removes 10 (smallest)

        // DEQUE - Double-ended queue (can add/remove from both ends)
        System.out.println("\n4. DEQUE Examples:");

        Deque<String> deque = new ArrayDeque<>();
        deque.addFirst("middle");
        deque.addFirst("front");
        deque.addLast("back");
        System.out.println("Deque: " + deque);
        System.out.println("Remove first: " + deque.removeFirst());
        System.out.println("Remove last: " + deque.removeLast());
        System.out.println("After removals: " + deque);

        // HASHMAP - Key-value pairs (not technically a Collection, but part of Collections framework)
        System.out.println("\n5. HASHMAP Examples:");

        HashMap<String, Integer> hashMap = new HashMap<>();
        hashMap.put("apple", 5);
        hashMap.put("banana", 3);
        hashMap.put("cherry", 8);
        System.out.println("HashMap: " + hashMap);
        System.out.println("Get 'banana': " + hashMap.get("banana"));

        TreeMap<String, Integer> treeMap = new TreeMap<>();
        treeMap.put("zebra", 1);
        treeMap.put("apple", 2);
        treeMap.put("banana", 3);
        System.out.println("TreeMap (sorted keys): " + treeMap);

        LinkedHashMap<String, Integer> linkedHashMap = new LinkedHashMap<>();
        linkedHashMap.put("first", 1);
        linkedHashMap.put("second", 2);
        linkedHashMap.put("third", 3);
        System.out.println("LinkedHashMap (insertion order): " + linkedHashMap);

        // PRACTICAL OPERATIONS
        System.out.println("\n=== PRACTICAL OPERATIONS ===");

        // Convert between collections
        List<String> fruits = Arrays.asList("apple", "banana", "cherry");
        Set<String> uniqueFruits = new HashSet<>(fruits);
        System.out.println("List to Set: " + uniqueFruits);

        // Iterate different ways
        System.out.println("\nIteration methods:");
        for (String fruit : fruits) {
            System.out.print(fruit + " ");
        }
        System.out.println();

        fruits.forEach(fruit -> System.out.print(fruit.toUpperCase() + " "));
        System.out.println();

        // Check operations
        System.out.println("\nChecking operations:");
        System.out.println("ArrayList contains 20: " + arrayList.contains(20));
        System.out.println("HashMap contains key 'apple': " + hashMap.containsKey("apple"));
        System.out.println("TreeSet size: " + treeSet.size());
        System.out.println("Queue is empty: " + queue.isEmpty());

        ArrayList<Integer> lst_numeros = new ArrayList<>();

        // Inserção dos elementos no ArrayList
        lst_numeros.add(10);
        lst_numeros.add(20);
        lst_numeros.add(30);
        lst_numeros.add(40);
        lst_numeros.add(50);

        // Acesso aos elementos no ArrayList
        System.out.println("Os elementos no ArrayList são:");
        for (int i = 0; i < lst_numeros.size(); i++) {
            System.out.println("lista["+i+"]= "+lst_numeros.get(i));
        }

        // Remove um elemento de um posição específica do ArrayList
        lst_numeros.remove(1);  // Remove o elemento da posição 2 do ArrayList

        // Alterar um elemento no ArrayList
        int x=57;
        lst_numeros.set(0, x);  // Coloca o elemento 57 na posição 0 do ArrayList

        // Verifica se o ArrayList contém um elemento específico
        int n = 100;
        String contem_elemento = lst_numeros.contains(n)?"Verdade":"Falso";
        System.out.println("O elemento "+n+" está the ArrayList? " + contem_elemento);

        // Iterar na lista através do laço for-each
        int k=0;
        System.out.println("Os elementos no ArrayList são:");
        lst_numeros.forEach(elemento -> System.out.println("= "+elemento));
        for (int elemento : lst_numeros) {
            System.out.println("lista["+k+"]= "+elemento);
            k++;
        }

        // Limpar o ArrayList de todos os elementos
        System.out.println("Limpar o ArrayList. ");
        lst_numeros.clear();

        // Verifica se o ArrayList está vazio
        String eh_vazio = lst_numeros.isEmpty()?"Verdade":"Falso";
        System.out.println("O ArrayList está vazio? " + eh_vazio);
    }
}