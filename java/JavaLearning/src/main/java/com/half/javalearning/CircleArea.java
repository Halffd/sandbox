package com.half.javalearning;

import java.util.Scanner;
import java.util.InputMismatchException;

/**
 * CircleAreaCalculator - A robust calculator for circle area computation
 *
 * Features comprehensive error handling, input validation, and proper resource management.
 * Handles edge cases like negative radii, invalid input, and extremely large/small values.
 */
public class CircleArea {

    private static final double MIN_RADIUS = 1e-10; // Minimum meaningful radius
    private static final double MAX_RADIUS = 1e10;  // Maximum reasonable radius
    private static final String DIVIDER = "==========================================";

    /**
     * Main method - Application entry point
     */
    public static void main(String[] args) {
        CircleArea calculator = new CircleArea();
        calculator.runCalculator();
    }

    /**
     * Main calculator method with proper resource management
     * Uses try-with-resources to ensure Scanner is properly closed
     */
    public void runCalculator() {
        printWelcomeMessage();

        // Use try-with-resources for automatic resource management
        try (Scanner scanner = new Scanner(System.in)) {

            boolean continueCalculating = true;

            while (continueCalculating) {
                try {
                    double radius = getValidRadius(scanner);
                    double area = calculateCircleArea(radius);
                    displayResults(radius, area);

                    continueCalculating = askToContinue(scanner);

                } catch (InvalidRadiusException e) {
                    System.err.println("❌ RADIUS ERROR: " + e.getMessage());
                    continueCalculating = askToContinue(scanner);

                } catch (InputMismatchException e) {
                    System.err.println("❌ INPUT ERROR: " + e.getMessage());
                    scanner.nextLine(); // Clear the invalid input
                    continueCalculating = askToContinue(scanner);

                } catch (Exception e) {
                    System.err.println("❌ UNEXPECTED ERROR: " + e.getMessage());
                    System.err.println("Please try again or contact support.");
                    continueCalculating = false;
                }

                System.out.println(DIVIDER);
            }

            System.out.println("Thank you for using the Circle Area Calculator!");

        } catch (Exception e) {
            System.err.println("FATAL ERROR: Unable to initialize input scanner");
            System.err.println("Details: " + e.getMessage());
        }
    }

    /**
     * Displays welcome message and instructions
     */
    private void printWelcomeMessage() {
        System.out.println("╔══════════════════════════════════════╗");
        System.out.println("║      CIRCLE AREA CALCULATOR         ║");
        System.out.println("║         Professional Edition         ║");
        System.out.println("╚══════════════════════════════════════╝");
        System.out.println("Calculate the area of a circle using the formula: A = π × r²");
        System.out.println("Features: Input validation, error handling, and precision control");
        System.out.println(DIVIDER);
    }

    /**
     * Gets a valid radius from user input with comprehensive validation
     *
     * @param scanner Scanner instance for input
     * @return Valid radius value
     * @throws InvalidRadiusException if radius is invalid
     * @throws InputMismatchException if input format is invalid
     */
    private double getValidRadius(Scanner scanner) throws InvalidRadiusException, InputMismatchException {
        System.out.print("🔢 Enter the radius of the circle: ");

        if (!scanner.hasNextDouble()) {
            String invalidInput = scanner.next();
            throw new InputMismatchException(
                    String.format("'%s' is not a valid number. Please enter a numeric value.", invalidInput)
            );
        }

        double radius = scanner.nextDouble();
        scanner.nextLine(); // Consume the newline

        // Validate radius constraints
        validateRadius(radius);

        return radius;
    }

    /**
     * Validates radius according to mathematical and practical constraints
     *
     * @param radius The radius to validate
     * @throws InvalidRadiusException if radius is invalid
     */
    private void validateRadius(double radius) throws InvalidRadiusException {
        if (Double.isNaN(radius)) {
            throw new InvalidRadiusException("Radius cannot be NaN (Not a Number)");
        }

        if (Double.isInfinite(radius)) {
            throw new InvalidRadiusException("Radius cannot be infinite");
        }

        if (radius <= 0) {
            throw new InvalidRadiusException(
                    String.format("Radius must be positive. Got: %.6f", radius)
            );
        }

        if (radius < MIN_RADIUS) {
            throw new InvalidRadiusException(
                    String.format("Radius too small (%.2e). Minimum allowed: %.2e",
                            radius, MIN_RADIUS)
            );
        }

        if (radius > MAX_RADIUS) {
            throw new InvalidRadiusException(
                    String.format("Radius too large (%.2e). Maximum allowed: %.2e",
                            radius, MAX_RADIUS)
            );
        }
    }

    /**
     * Calculates the area of a circle using the formula A = π × r²
     *
     * @param radius The radius of the circle
     * @return The calculated area
     * @throws ArithmeticException if calculation results in overflow
     */
    private double calculateCircleArea(double radius) throws ArithmeticException {
        double area = Math.PI * radius * radius;

        // Check for calculation overflow
        if (Double.isInfinite(area)) {
            throw new ArithmeticException(
                    String.format("Area calculation resulted in overflow for radius: %.6f", radius)
            );
        }

        if (Double.isNaN(area)) {
            throw new ArithmeticException(
                    String.format("Area calculation resulted in NaN for radius: %.6f", radius)
            );
        }

        return area;
    }

    /**
     * Displays calculation results with additional insights
     *
     * @param radius The input radius
     * @param area The calculated area
     */
    private void displayResults(double radius, double area) {
        System.out.println("\n✅ CALCULATION SUCCESSFUL!");
        System.out.println("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");

        // Primary result
        System.out.printf("🔵 Circle radius: %.6f units%n", radius);
        System.out.printf("📐 Circle area:   %.6f square units%n", area);

        // Additional calculations and insights
        double circumference = 2 * Math.PI * radius;
        double diameter = 2 * radius;

        System.out.println("\n📊 ADDITIONAL INFORMATION:");
        System.out.printf("   Diameter:      %.6f units%n", diameter);
        System.out.printf("   Circumference: %.6f units%n", circumference);
        System.out.printf("   Area/Radius²:  %.6f (should be π = %.6f)%n",
                area / (radius * radius), Math.PI);

        // Scientific notation for very large/small values
        if (area > 1e6 || area < 1e-6) {
            System.out.printf("   Area (scientific): %.6e square units%n", area);
        }

        // Practical comparisons
        if (area > 1000000) {
            System.out.printf("   💡 That's %.2f square kilometers!%n", area / 1000000);
        } else if (area > 10000) {
            System.out.printf("   💡 That's %.2f hectares!%n", area / 10000);
        }

        System.out.println("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    }

    /**
     * Asks user if they want to perform another calculation
     *
     * @param scanner Scanner instance for input
     * @return true if user wants to continue, false otherwise
     */
    private boolean askToContinue(Scanner scanner) {
        System.out.print("\n🔄 Calculate another circle area? (y/n): ");
        String response = scanner.nextLine().trim().toLowerCase();

        return response.equals("y") || response.equals("yes") ||
                response.equals("s") || response.equals("si");
    }

    /**
     * Custom exception for invalid radius values
     */
    public static class InvalidRadiusException extends Exception {
        public InvalidRadiusException(String message) {
            super(message);
        }

        public InvalidRadiusException(String message, Throwable cause) {
            super(message, cause);
        }
    }
}