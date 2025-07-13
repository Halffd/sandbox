package com.half.javalearning.exceptions;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

public class Fibonacci {
    private static final Map<Long, Long> memo = new ConcurrentHashMap<>();
    private static final long MAX_SAFE_INPUT = 92L;

    // Same custom exceptions
    public static class FibonacciException extends Exception {
        public FibonacciException(String message) { super(message); }
    }

    public static class FibonacciOverflowException extends FibonacciException {
        public FibonacciOverflowException(String message) { super(message); }
    }

    public static class InvalidFibonacciInputException extends FibonacciException {
        public InvalidFibonacciInputException(String message) { super(message); }
    }

    // Fixed memoization - no more recursive update drama
    public static long fibonacciMemo(long n) throws FibonacciException {
        if (n < 0) {
            throw new InvalidFibonacciInputException("Negative numbers? Really? n=" + n);
        }
        if (n > MAX_SAFE_INPUT) {
            throw new FibonacciOverflowException("Input too large, will overflow: " + n);
        }

        if (n <= 1) return n;

        // Check if already computed
        Long cached = memo.get(n);
        if (cached != null) return cached;

        // Compute and store - no recursive lambda bullshit
        long result = fibonacciMemo(n - 1) + fibonacciMemo(n - 2);
        memo.put(n, result);
        return result;
    }

    // Tail recursive version stays the same - it was already based
    public static long fibonacciTailRec(long n) throws FibonacciException {
        if (n < 0) {
            throw new InvalidFibonacciInputException("Still no negatives, chief: " + n);
        }
        if (n > MAX_SAFE_INPUT) {
            throw new FibonacciOverflowException("Still gonna overflow: " + n);
        }

        return fibTailHelper(n, 0, 1);
    }

    private static long fibTailHelper(long n, long a, long b) {
        if (n == 0) return a;
        if (n == 1) return b;
        return fibTailHelper(n - 1, b, a + b);
    }

    // Same main method as before
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);

        System.out.println("Enter a number for Fibonacci (or -1 to quit):");

        while (true) {
            try {
                System.out.print("> ");
                long input = scanner.nextLong();

                if (input == -1) {
                    System.out.println("Peace out!");
                    break;
                }

                long startTime = System.nanoTime();
                long result = fibonacciMemo(input);
                long endTime = System.nanoTime();

                System.out.printf("F(%d) = %d (took %.3f ms)%n",
                        input, result, (endTime - startTime) / 1_000_000.0);

                startTime = System.nanoTime();
                long tailResult = fibonacciTailRec(input);
                endTime = System.nanoTime();

                System.out.printf("Tail rec: F(%d) = %d (took %.3f ms)%n",
                        input, tailResult, (endTime - startTime) / 1_000_000.0);

            } catch (InvalidFibonacciInputException e) {
                System.err.println("Input Error: " + e.getMessage());
            } catch (FibonacciOverflowException e) {
                System.err.println("Overflow Error: " + e.getMessage());
            } catch (InputMismatchException e) {
                System.err.println("That's not a number, genius");
                scanner.nextLine();
            } catch (Exception e) {
                System.err.println("Something went wrong: " + e.getMessage());
                e.printStackTrace(); // Debug info for the next time I fuck up
            }
        }

        scanner.close();
    }
}