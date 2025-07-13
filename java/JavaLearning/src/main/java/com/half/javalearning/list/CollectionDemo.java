package com.half.javalearning.list;
import java.util.*;
import java.util.concurrent.LinkedBlockingQueue;

public class CollectionDemo {
    public static void main(String[] args) {
        System.out.println("=== JAVA COLLECTIONS FRAMEWORK DEMO ===\n");

        // Core Collection interface methods demo
        demonstrateCollectionMethods();

        // Specialized interface behaviors
        demonstrateSet();
        demonstrateList();
        demonstrateQueue();
        demonstrateDeque();

        // Map (not part of Collection hierarchy but essential)
        demonstrateMap();
    }

    // Core Collection interface methods
    private static void demonstrateCollectionMethods() {
        System.out.println("🔧 CORE COLLECTION METHODS:");
        Collection<String> col = new ArrayList<>();

        // add(Object e) - adds element
        col.add("Java");
        col.add("Python");
        System.out.println("After add(): " + col);

        // addAll(Collection c) - adds all elements from another collection
        Collection<String> more = Arrays.asList("C++", "Rust", "Go");
        col.addAll(more);
        System.out.println("After addAll(): " + col);

        // contains(Object c) - checks if element exists
        System.out.println("Contains 'Java': " + col.contains("Java"));
        System.out.println("Contains 'COBOL': " + col.contains("COBOL"));

        // containsAll(Collection c) - checks if all elements exist
        System.out.println("Contains all ['Java', 'Python']: " +
                col.containsAll(Arrays.asList("Java", "Python")));

        // size() and isEmpty()
        System.out.println("Size: " + col.size());
        System.out.println("Is empty: " + col.isEmpty());

        // iterator() - for traversing
        System.out.print("Iterator traversal: ");
        Iterator<String> it = col.iterator();
        while (it.hasNext()) {
            System.out.print(it.next() + " ");
        }
        System.out.println();

        // toArray() variants
        Object[] array1 = col.toArray();
        String[] array2 = col.toArray(new String[0]);
        System.out.println("toArray(): " + Arrays.toString(array1));
        System.out.println("toArray(T[]): " + Arrays.toString(array2));

        // remove operations
        col.remove("Go");
        System.out.println("After remove('Go'): " + col);

        col.removeAll(Arrays.asList("C++", "Rust"));
        System.out.println("After removeAll(): " + col);

        // retainAll - keeps only specified elements
        col.addAll(Arrays.asList("JavaScript", "TypeScript", "Kotlin"));
        col.retainAll(Arrays.asList("Java", "Python", "Kotlin"));
        System.out.println("After retainAll(): " + col);

        // clear() - removes all elements
        Collection<String> temp = new ArrayList<>(col);
        temp.clear();
        System.out.println("After clear(): " + temp + " (isEmpty: " + temp.isEmpty() + ")");

        // equals() and hashCode()
        Collection<String> col2 = new ArrayList<>(Arrays.asList("Java", "Python", "Kotlin"));
        System.out.println("Collections equal: " + col.equals(col2));
        System.out.println("HashCodes - col1: " + col.hashCode() + ", col2: " + col2.hashCode());

        System.out.println("\n" + "=".repeat(50) + "\n");
    }

    // SET - No duplicates, mathematical set operations
    private static void demonstrateSet() {
        System.out.println("📝 SET INTERFACE (No duplicates):");

        // HashSet - fast, no order guarantee
        Set<String> hashSet = new HashSet<>();
        hashSet.addAll(Arrays.asList("apple", "banana", "apple", "cherry"));
        System.out.println("HashSet (no duplicates, no order): " + hashSet);

        // LinkedHashSet - insertion order preserved
        Set<String> linkedSet = new LinkedHashSet<>();
        linkedSet.addAll(Arrays.asList("apple", "banana", "apple", "cherry"));
        System.out.println("LinkedHashSet (insertion order): " + linkedSet);

        // TreeSet - sorted order
        Set<Integer> treeSet = new TreeSet<>();
        treeSet.addAll(Arrays.asList(5, 1, 9, 3, 7, 1, 5));
        System.out.println("TreeSet (sorted, no duplicates): " + treeSet);

        // Set operations
        Set<String> set1 = new HashSet<>(Arrays.asList("Java", "Python", "C++"));
        Set<String> set2 = new HashSet<>(Arrays.asList("Python", "JavaScript", "Go"));

        // Union
        Set<String> union = new HashSet<>(set1);
        union.addAll(set2);
        System.out.println("Union: " + union);

        // Intersection
        Set<String> intersection = new HashSet<>(set1);
        intersection.retainAll(set2);
        System.out.println("Intersection: " + intersection);

        // Difference
        Set<String> difference = new HashSet<>(set1);
        difference.removeAll(set2);
        System.out.println("Difference (set1 - set2): " + difference);

        System.out.println("\n" + "=".repeat(50) + "\n");
    }

    // LIST - Ordered, allows duplicates, indexed access
    private static void demonstrateList() {
        System.out.println("📋 LIST INTERFACE (Ordered, indexed, allows duplicates):");

        // ArrayList - resizable array
        List<String> arrayList = new ArrayList<>();
        arrayList.addAll(Arrays.asList("first", "second", "third", "second"));
        System.out.println("ArrayList: " + arrayList);

        // LinkedList - doubly-linked list
        List<Integer> linkedList = new LinkedList<>();
        linkedList.addAll(Arrays.asList(10, 20, 30, 20));
        System.out.println("LinkedList: " + linkedList);

        // List-specific methods
        System.out.println("Element at index 1: " + arrayList.get(1));
        arrayList.set(1, "MODIFIED");
        System.out.println("After set(1, 'MODIFIED'): " + arrayList);

        arrayList.add(2, "INSERTED");
        System.out.println("After add(2, 'INSERTED'): " + arrayList);

        System.out.println("Index of 'third': " + arrayList.indexOf("third"));
        System.out.println("Last index of 'second': " + arrayList.lastIndexOf("second"));

        // Sublist
        List<String> subList = arrayList.subList(1, 4);
        System.out.println("Sublist(1,4): " + subList);

        // List iteration with ListIterator (bidirectional)
        System.out.print("Reverse iteration: ");
        ListIterator<String> listIt = arrayList.listIterator(arrayList.size());
        while (listIt.hasPrevious()) {
            System.out.print(listIt.previous() + " ");
        }
        System.out.println();

        System.out.println("\n" + "=".repeat(50) + "\n");
    }

    // QUEUE - FIFO (First In, First Out)
    private static void demonstrateQueue() {
        System.out.println("🚶 QUEUE INTERFACE (FIFO - First In, First Out):");

        // LinkedList as Queue
        Queue<String> queue = new LinkedList<>();

        // offer() - adds element (similar to add but better for capacity-restricted queues)
        queue.offer("First");
        queue.offer("Second");
        queue.offer("Third");
        System.out.println("Queue after offers: " + queue);

        // peek() - returns head without removing
        System.out.println("Peek (head): " + queue.peek());
        System.out.println("Queue after peek: " + queue);

        // poll() - removes and returns head
        System.out.println("Poll (remove head): " + queue.poll());
        System.out.println("Queue after poll: " + queue);

        // element() vs peek() - element() throws exception if empty
        System.out.println("Element (head, throws if empty): " + queue.element());

        // remove() vs poll() - remove() throws exception if empty
        System.out.println("Remove (head, throws if empty): " + queue.remove());
        System.out.println("Queue after remove: " + queue);

        // PriorityQueue - elements ordered by priority
        Queue<Integer> priorityQueue = new PriorityQueue<>();
        priorityQueue.addAll(Arrays.asList(5, 1, 9, 3, 7));
        System.out.println("PriorityQueue (min-heap): " + priorityQueue);
        System.out.println("Poll from PriorityQueue: " + priorityQueue.poll()); // Gets minimum

        // BlockingQueue example
        Queue<String> blockingQueue = new LinkedBlockingQueue<>(2); // Capacity 2
        blockingQueue.offer("A");
        blockingQueue.offer("B");
        System.out.println("BlockingQueue: " + blockingQueue);
        System.out.println("Can offer more? " + blockingQueue.offer("C")); // false - full

        System.out.println("\n" + "=".repeat(50) + "\n");
    }

    // DEQUE - Double-ended queue (can add/remove from both ends)
    private static void demonstrateDeque() {
        System.out.println("🔄 DEQUE INTERFACE (Double-ended queue):");

        Deque<String> deque = new ArrayDeque<>();

        // Add to front and back
        deque.addFirst("Middle");
        deque.addLast("End");
        deque.addFirst("Start");
        System.out.println("After adding to both ends: " + deque);

        // offer variants
        deque.offerFirst("New Start");
        deque.offerLast("New End");
        System.out.println("After offer operations: " + deque);

        // peek variants
        System.out.println("Peek first: " + deque.peekFirst());
        System.out.println("Peek last: " + deque.peekLast());

        // poll variants
        System.out.println("Poll first: " + deque.pollFirst());
        System.out.println("Poll last: " + deque.pollLast());
        System.out.println("Deque after polls: " + deque);

        // Using as Stack (LIFO)
        System.out.println("\nUsing Deque as Stack:");
        Deque<Integer> stack = new ArrayDeque<>();
        stack.push(1);
        stack.push(2);
        stack.push(3);
        System.out.println("Stack after pushes: " + stack);
        System.out.println("Pop: " + stack.pop());
        System.out.println("Stack after pop: " + stack);

        System.out.println("\n" + "=".repeat(50) + "\n");
    }

    // MAP - Key-Value pairs (not part of Collection hierarchy but essential)
    private static void demonstrateMap() {
        System.out.println("🗺️ MAP INTERFACE (Key-Value pairs):");

        // HashMap - fast, no order guarantee
        Map<String, Integer> hashMap = new HashMap<>();
        hashMap.put("Java", 25);
        hashMap.put("Python", 30);
        hashMap.put("JavaScript", 27);
        System.out.println("HashMap: " + hashMap);

        // LinkedHashMap - insertion order preserved
        Map<String, Integer> linkedMap = new LinkedHashMap<>();
        linkedMap.put("Third", 3);
        linkedMap.put("First", 1);
        linkedMap.put("Second", 2);
        System.out.println("LinkedHashMap (insertion order): " + linkedMap);

        // TreeMap - sorted by keys
        Map<String, Integer> treeMap = new TreeMap<>();
        treeMap.put("Zebra", 26);
        treeMap.put("Apple", 1);
        treeMap.put("Banana", 2);
        System.out.println("TreeMap (sorted keys): " + treeMap);

        // Map operations
        System.out.println("Get 'Java': " + hashMap.get("Java"));
        System.out.println("Contains key 'Python': " + hashMap.containsKey("Python"));
        System.out.println("Contains value 30: " + hashMap.containsValue(30));

        // getOrDefault
        System.out.println("Get 'C++' (default 0): " + hashMap.getOrDefault("C++", 0));

        // putIfAbsent
        hashMap.putIfAbsent("C++", 20);
        System.out.println("After putIfAbsent: " + hashMap);

        // replace operations
        hashMap.replace("Java", 25, 28); // replace if current value matches
        System.out.println("After conditional replace: " + hashMap);

        // Iterating over maps
        System.out.println("\nMap iteration methods:");
        System.out.println("Keys: " + hashMap.keySet());
        System.out.println("Values: " + hashMap.values());
        System.out.println("Entries: " + hashMap.entrySet());

        // forEach with lambda
        System.out.print("Lambda iteration: ");
        hashMap.forEach((k, v) -> System.out.print(k + "=" + v + " "));
        System.out.println();
    }
}