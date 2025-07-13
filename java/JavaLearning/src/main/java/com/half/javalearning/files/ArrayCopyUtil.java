package com.half.javalearning.files;

// The main class, but not braindead this time
public class ArrayCopyUtil {

    public static void copyArray(char[] source, char[] destination) {
        // Validate inputs like a normal person
        validateArrays(source, destination);

        // Actually copy the damn array
        System.arraycopy(source, 0, destination, 0, source.length);
        System.out.println("Array copied successfully!");
    }

    private static void validateArrays(char[] source, char[] destination) {
        // Check for null like a human being
        if (source == null) {
            throw new ArrayCopyException("Source array cannot be null");
        }

        if (destination == null) {
            throw new ArrayCopyException("Destination array cannot be null");
        }

        // Check size compatibility without the wrapper hell
        if (source.length > destination.length) {
            throw new ArrayCopyException(
                    String.format("Insufficient space in destination array! Need %d, got %d",
                            source.length, destination.length)
            );
        }
    }

    public static void main(String[] args) {
        char[] source = {'a', 'b', 'c', 'd', 'e'};
        char[] destination = {'x', 'y', 'z'};  // Too small, will fail

        try {
            copyArray(source, destination);
        } catch (ArrayCopyException e) {
            System.err.println("Copy failed: " + e.getMessage());
            // Actually handle the error instead of rethrowing like a maniac
        }

        // Example with correct sizing
        System.out.println("\nTrying with correct size:");
        char[] biggerDestination = new char[source.length];

        try {
            copyArray(source, biggerDestination);
            System.out.println("Result: " + String.valueOf(biggerDestination));
        } catch (ArrayCopyException e) {
            System.err.println("This shouldn't happen: " + e.getMessage());
        }
    }
}