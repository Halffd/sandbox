package com.half.javalearning;

public class MathUtility {
    // Class constant shared by all instances
    public static final double PI_APPROX = 3.14159;

    // This entire class will use strict floating point calculations
    public static strictfp class PreciseCalculator {
        // Static counter to track how many calculations performed
        private static int calculationsPerformed = 0;

        // Static and strictly consistent floating point method
        public static double complexTrigCalculation(double angle) {
            calculationsPerformed++;
            return (Math.sin(angle) * Math.cos(angle)) / (Math.tan(angle) + 0.0001);
        }

        // Static method to access our counter
        public static int getCalculationCount() {
            return calculationsPerformed;
        }
    }

    public static void calc(String[] args) {
        // Use our static method without instantiation
        double result1 = PreciseCalculator.complexTrigCalculation(PI_APPROX/4);
        double result2 = PreciseCalculator.complexTrigCalculation(PI_APPROX/3);

        System.out.println("Result 1: " + result1);
        System.out.println("Result 2: " + result2);
        System.out.println("Calculations performed: " +
                PreciseCalculator.getCalculationCount());
    }
}
