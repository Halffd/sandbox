package com.half.javalearning;
import java.util.*;
import java.util.function.Predicate;

public class PredicateWildcardDemo {
    
    public static void main(String[] args) {
        PredicateWildcardDemo demo = new PredicateWildcardDemo();
        demo.runDemo();
    }
    
    public void runDemo() {
        System.out.println("=== Predicate Wildcards Demo ===\n");
        
        // Basic predicates
        Predicate<String> isLongName = name -> name.length() > 5;
        Predicate<Integer> isEven = num -> num % 2 == 0;
        Predicate<Object> isNotNull = obj -> obj != null;
        
        // Test basic predicates
        System.out.println("Basic Predicates:");
        System.out.println("'Bob' is long: " + isLongName.test("Bob"));
        System.out.println("'Alexander' is long: " + isLongName.test("Alexander"));
        System.out.println("4 is even: " + isEven.test(4));
        System.out.println("null is not null: " + isNotNull.test(null));
        
        System.out.println("\n=== Wildcard Examples ===");
        
        // ? extends T examples (PRODUCERS - they give you stuff)
        demonstrateExtendsWildcard();
        
        // ? super T examples (CONSUMERS - they take stuff)
        demonstrateSuperWildcard();
        
        // Combined example
        demonstrateRealWorldScenario();
    }
    
    // ? extends T = "T or more specific" - PRODUCER
    // Use this when you're READING from the collection
    private void demonstrateExtendsWildcard() {
        System.out.println("\n--- ? extends T (PRODUCER) ---");
        
        List<String> strings = Arrays.asList("short", "medium", "supercalifragilisticexpialidocious");
        List<Integer> numbers = Arrays.asList(1, 22, 333, 4444);
        
        // This method can accept List<String>, List<Integer>, or any List<? extends Object>
        // because both String and Integer extend Object
        processItems(strings, obj -> obj.toString().length() > 6);
        processItems(numbers, obj -> obj.toString().contains("3"));
        
        // Why extends? Because we're PRODUCING/READING items from the list
        // The method doesn't know the exact type, but knows it's "T or smaller"
    }
    
    // Method that READS from a list (producer scenario)
    private void processItems(List<? extends Object> items, Predicate<Object> predicate) {
        System.out.println("Processing items that match predicate:");
        for (Object item : items) {
            if (predicate.test(item)) {
                System.out.println("  Match: " + item);
            }
        }
    }
    
    // ? super T = "T or more general" - CONSUMER  
    // Use this when you're WRITING to the collection
    private void demonstrateSuperWildcard() {
        System.out.println("\n--- ? super T (CONSUMER) ---");
        
        List<Object> objects = new ArrayList<>();
        List<Number> numbers = new ArrayList<>();
        
        // These methods CONSUME/ACCEPT items - they add stuff to collections
        addValidItems(objects, Arrays.asList("test", 42, 3.14, null), obj -> obj != null);
        addValidItems(numbers, Arrays.asList(1, 2.5, 42L), num -> num.doubleValue() > 2.0);
        
        System.out.println("Objects after filtering: " + objects);
        System.out.println("Numbers after filtering: " + numbers);
    }
    
    // Method that WRITES to a list (consumer scenario)
    private <T> void addValidItems(List<? super T> destination, List<T> source, Predicate<T> validator) {
        System.out.println("Adding valid items...");
        for (T item : source) {
            if (validator.test(item)) {
                destination.add(item); // CONSUMING - adding to the list
                System.out.println("  Added: " + item);
            }
        }
    }
    
    // Real-world example combining both
    private void demonstrateRealWorldScenario() {
        System.out.println("\n--- Real World: Validation Pipeline ---");
        
        // Source data (we READ from this - extends)
        List<String> usernames = Arrays.asList("bob", "alice123", "admin", "x", "superuser");
        
        // Destination (we WRITE to this - super)  
        List<Object> validUsernames = new ArrayList<>();
        
        // Validation predicates
        Predicate<String> isValidLength = name -> name.length() >= 3 && name.length() <= 20;
        Predicate<String> hasNoNumbers = name -> !name.matches(".*\\d.*");
        Predicate<String> isNotReserved = name -> !Arrays.asList("admin", "root", "system").contains(name);
        
        // Combined predicate
        Predicate<String> isValidUsername = isValidLength.and(hasNoNumbers).and(isNotReserved);
        
        // Filter and collect valid usernames
        filterAndCollect(usernames, validUsernames, isValidUsername);
        
        System.out.println("Valid usernames: " + validUsernames);
    }
    
    // Generic method showing both wildcards in action
    private <T> void filterAndCollect(List<? extends T> source,      // PRODUCER - gives us T's
                                     List<? super T> destination,    // CONSUMER - accepts T's  
                                     Predicate<T> filter) {
        
        System.out.println("Filtering items...");
        for (T item : source) {           // Reading from source (extends)
            if (filter.test(item)) {
                destination.add(item);     // Writing to destination (super)
                System.out.println("  Passed: " + item);
            } else {
                System.out.println("  Failed: " + item);
            }
        }
    }
}