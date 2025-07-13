package com.half.javalearning.exceptions;

import java.util.InputMismatchException;
import java.util.Scanner;

/**
 * AdvancedMathCalculator - A comprehensive calculator with multiple mathematical operations
 *
 * Supports: Division, Square Root, Power, and Modulus operations
 * Features robust error handling, input validation, and user-friendly interface
 */
public class Calculator {

    private static final String CONTINUE_PROMPT = "s";
    private static final String DIVIDER_LINE = "==========================================";
    private static final double EPSILON = 1e-10; // For floating point comparisons

    // Menu options
    private enum Operation {
        DIVISION(1, "Division (a ÷ b)"),
        SQUARE_ROOT(2, "Square Root (√x)"),
        POWER(3, "Power (x^y)"),
        MODULUS(4, "Modulus (a % b)"),
        EXIT(5, "Exit");

        private final int code;
        private final String description;

        Operation(int code, String description) {
            this.code = code;
            this.description = description;
        }

        public int getCode() { return code; }
        public String getDescription() { return description; }

        public static Operation fromCode(int code) {
            for (Operation op : values()) {
                if (op.code == code) return op;
            }
            throw new IllegalArgumentException("Invalid operation code: " + code);
        }
    }

    /**
     * Main method - Application entry point
     */
    public static void main(String[] args) {
        Calculator calculator = new Calculator();
        calculator.runCalculator();
    }

    /**
     * Main calculator loop with menu-driven interface
     */
    public void runCalculator() {
        printWelcomeMessage();

        try (Scanner scanner = new Scanner(System.in)) {
            boolean keepRunning = true;

            while (keepRunning) {
                try {
                    displayMenu();
                    int choice = getValidInteger(scanner, "Select an operation (1-5): ");

                    Operation operation = Operation.fromCode(choice);

                    if (operation == Operation.EXIT) {
                        keepRunning = false;
                        System.out.println("Thank you for using the Advanced Math Calculator!");
                        break;
                    }

                    executeOperation(scanner, operation);

                } catch (IllegalArgumentException e) {
                    System.err.println("ERROR: " + e.getMessage());
                    System.err.println("Please select a valid option (1-5).");

                } catch (Exception e) {
                    System.err.println("ERROR: An unexpected error occurred: " + e.getMessage());
                    System.err.println("Please try again.");
                }

                System.out.println(DIVIDER_LINE);
            }

        } catch (Exception e) {
            System.err.println("FATAL ERROR: " + e.getMessage());
            System.err.println("Application will now exit.");
        }
    }

    /**
     * Displays welcome message and instructions
     */
    private void printWelcomeMessage() {
        System.out.println("╔══════════════════════════════════════╗");
        System.out.println("║    ADVANCED MATHEMATICAL CALCULATOR ║");
        System.out.println("║         Safe & Robust Edition        ║");
        System.out.println("╚══════════════════════════════════════╝");
        System.out.println("Features comprehensive error handling and input validation");
        System.out.println(DIVIDER_LINE);
    }

    /**
     * Displays the operation menu
     */
    private void displayMenu() {
        System.out.println("\n📋 AVAILABLE OPERATIONS:");
        for (Operation op : Operation.values()) {
            System.out.printf("  %d. %s%n", op.getCode(), op.getDescription());
        }
        System.out.println();
    }

    /**
     * Executes the selected mathematical operation
     */
    private void executeOperation(Scanner scanner, Operation operation) {
        System.out.println("🔢 Selected: " + operation.getDescription());
        System.out.println();

        try {
            switch (operation) {
                case DIVISION:
                    performDivision(scanner);
                    break;
                case SQUARE_ROOT:
                    performSquareRoot(scanner);
                    break;
                case POWER:
                    performPower(scanner);
                    break;
                case MODULUS:
                    performModulus(scanner);
                    break;
                default:
                    throw new IllegalStateException("Unhandled operation: " + operation);
            }
        } catch (ArithmeticException e) {
            System.err.println("❌ MATHEMATICAL ERROR: " + e.getMessage());
        } catch (InputMismatchException e) {
            System.err.println("❌ INPUT ERROR: " + e.getMessage());
            scanner.nextLine(); // Clear buffer
        } catch(NullPointerException e){
            System.err.println("<UNK> NULL ERROR: " + e.getMessage());
            scanner.nextLine();
        } catch (Exception e) {
            System.err.println("❌ OPERATION ERROR: " + e.getMessage());
        }
    }

    /**
     * Performs division operation with comprehensive error handling
     */
    private void performDivision(Scanner scanner) {
        double dividend = getValidDouble(scanner, "Enter dividend (number to be divided): ");
        double divisor = getValidDouble(scanner, "Enter divisor (number to divide by): ");
        try {
            if ( divisor ==0 )
                throw new ArithmeticException ( "Null Divisor." );
        }
        catch (Exception e)
        {
            System.out.println( "ERRO: Divisão por zero! " + e.getMessage() );
            throw e;
        }
        if (Math.abs(divisor) < EPSILON) {
            throw new ArithmeticException("Division by zero is undefined in mathematics");
        }

        double result = dividend / divisor;

        System.out.printf("✅ RESULT: %.6f ÷ %.6f = %.10f%n", dividend, divisor, result);

        // Additional insights
        if (Math.abs(result - Math.round(result)) < EPSILON) {
            System.out.println("💡 Note: Result is a whole number");
        }

        if (dividend != 0 && Math.abs(divisor) > Math.abs(dividend)) {
            System.out.println("💡 Note: Result is a fraction (|result| < 1)");
        }
    }

    /**
     * Performs square root operation with domain validation
     */
    private void performSquareRoot(Scanner scanner) {
        double number = getValidDouble(scanner, "Enter number for square root: ");

        if (number < 0) {
            throw new ArithmeticException(
                    String.format("Square root of negative number (%.6f) is not real. " +
                            "Result would be %.6fi (imaginary)", number, Math.sqrt(-number))
            );
        }

        double result = Math.sqrt(number);

        System.out.printf("✅ RESULT: √%.6f = %.10f%n", number, result);

        // Additional insights
        if (Math.abs(number - 1) < EPSILON) {
            System.out.println("💡 Note: √1 = 1 (identity property)");
        } else if (Math.abs(number) < EPSILON) {
            System.out.println("💡 Note: √0 = 0");
        } else if (isPerfectSquare(number)) {
            System.out.println("💡 Note: This is a perfect square!");
        }

        // Verification
        double verification = result * result;
        System.out.printf("🔍 Verification: (%.10f)² = %.10f%n", result, verification);
    }

    /**
     * Performs power operation with overflow protection
     */
    private void performPower(Scanner scanner) {
        double base = getValidDouble(scanner, "Enter base (x): ");
        double exponent = getValidDouble(scanner, "Enter exponent (y): ");

        // Check for problematic cases
        if (Math.abs(base) < EPSILON && exponent < 0) {
            throw new ArithmeticException("0 raised to negative power is undefined (division by zero)");
        }

        if (base < 0 && !isInteger(exponent)) {
            throw new ArithmeticException(
                    String.format("Negative base (%.6f) with non-integer exponent (%.6f) " +
                            "results in complex number", base, exponent)
            );
        }

        double result = Math.pow(base, exponent);

        // Check for overflow/underflow
        if (Double.isInfinite(result)) {
            throw new ArithmeticException("Result is too large (overflow to infinity)");
        }

        if (Double.isNaN(result)) {
            throw new ArithmeticException("Result is not a number (NaN)");
        }

        System.out.printf("✅ RESULT: %.6f^%.6f = %.10f%n", base, exponent, result);

        // Additional insights
        if (Math.abs(exponent - 1) < EPSILON) {
            System.out.println("💡 Note: Any number to the power of 1 equals itself");
        } else if (Math.abs(exponent) < EPSILON) {
            System.out.println("💡 Note: Any non-zero number to the power of 0 equals 1");
        } else if (Math.abs(exponent - 2) < EPSILON) {
            System.out.println("💡 Note: This is a square operation");
        }

        // Scientific notation for very large/small numbers
        if (Math.abs(result) > 1e6 || (Math.abs(result) < 1e-6 && Math.abs(result) > 0)) {
            System.out.printf("🔬 Scientific notation: %.6e%n", result);
        }
    }

    /**
     * Performs modulus operation with proper validation
     */
    private void performModulus(Scanner scanner) {
        // For modulus, we typically work with integers for meaningful results
        System.out.println("💡 Note: Modulus typically works with integers for meaningful results");

        double dividend = getValidDouble(scanner, "Enter dividend (number to be divided): ");
        double divisor = getValidDouble(scanner, "Enter divisor (modulus): ");

        if (Math.abs(divisor) < EPSILON) {
            throw new ArithmeticException("Modulus by zero is undefined");
        }

        double result = dividend % divisor;

        System.out.printf("✅ RESULT: %.6f %% %.6f = %.10f%n", dividend, divisor, result);

        // Additional insights
        if (Math.abs(result) < EPSILON) {
            System.out.println("💡 Note: Dividend is perfectly divisible by divisor");
        }

        // If inputs were integers, show integer interpretation
        if (isInteger(dividend) && isInteger(divisor)) {
            int intDividend = (int) dividend;
            int intDivisor = (int) divisor;
            int intResult = intDividend % intDivisor;

            System.out.printf("🔢 Integer interpretation: %d %% %d = %d%n",
                    intDividend, intDivisor, intResult);

            // Division breakdown
            int quotient = intDividend / intDivisor;
            System.out.printf("📊 Breakdown: %d = (%d × %d) + %d%n",
                    intDividend, intDivisor, quotient, intResult);
        }
    }

    /**
     * Gets a valid integer with comprehensive error handling
     */
    private int getValidInteger(Scanner scanner, String prompt) throws InputMismatchException {
        System.out.print(prompt);

        if (!scanner.hasNextInt()) {
            String input = scanner.next();
            throw new InputMismatchException("'" + input + "' is not a valid integer");
        }

        int value = scanner.nextInt();
        scanner.nextLine(); // Consume newline

        return value;
    }

    /**
     * Gets a valid double with comprehensive error handling
     */
    private double getValidDouble(Scanner scanner, String prompt) throws InputMismatchException {
        System.out.print(prompt);

        if (!scanner.hasNextDouble()) {
            String input = scanner.next();
            throw new InputMismatchException("'" + input + "' is not a valid number");
        }

        double value = scanner.nextDouble();
        scanner.nextLine(); // Consume newline

        // Check for special values
        if (Double.isNaN(value)) {
            throw new InputMismatchException("Input resulted in NaN (Not a Number)");
        }

        if (Double.isInfinite(value)) {
            throw new InputMismatchException("Input resulted in infinite value");
        }

        return value;
    }

    /**
     * Checks if a double value is effectively an integer
     */
    private boolean isInteger(double value) {
        return Math.abs(value - Math.round(value)) < EPSILON;
    }

    /**
     * Checks if a number is a perfect square
     */
    private boolean isPerfectSquare(double number) {
        if (number < 0) return false;

        double sqrt = Math.sqrt(number);
        return Math.abs(sqrt - Math.round(sqrt)) < EPSILON;
    }
}