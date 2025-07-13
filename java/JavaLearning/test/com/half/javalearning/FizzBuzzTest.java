package com.half.javalearning;

import org.junit.jupiter.api.*;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Test class for FizzBuzz implementation
 */
class FizzBuzzTest {

    private FizzBuzz fizzBuzz;
    private ByteArrayOutputStream outputStream;
    private PrintStream originalOut;

    /**
     * Set up before each test
     */
    @BeforeEach
    void setUp() {
        fizzBuzz = new FizzBuzz();
        outputStream = new ByteArrayOutputStream();
        originalOut = System.out;
        System.setOut(new PrintStream(outputStream)); // Redirect output
    }

    /**
     * Restore System.out after each test
     */
    @AfterEach
    void tearDown() {
        System.setOut(originalOut); // Restore original System.out
    }

    /**
     * Parameterized test for validating FizzBuzz output
     */
    @Test
    @DisplayName("Test FizzBuzz numbers for small range")
    void testFizzBuzzOutput() {
        fizzBuzz.fizzBuzz(15);
        String expected =
                "1\n" +
                        "2\n" +
                        "Fizz\n" +
                        "4\n" +
                        "Buzz\n" +
                        "Fizz\n" +
                        "7\n" +
                        "8\n" +
                        "Fizz\n" +
                        "Buzz\n" +
                        "11\n" +
                        "Fizz\n" +
                        "13\n" +
                        "14\n" +
                        "FizzBuzz\n";

        assertEquals(expected, outputStream.toString());
    }

    @Test
    @DisplayName("Test multiples of 3")
    void testMultiplesOfThree() {
        fizzBuzz.fizzBuzz(6);
        String expected =
                "1\n" +
                        "2\n" +
                        "Fizz\n" +
                        "4\n" +
                        "Buzz\n" +
                        "Fizz\n";

        assertEquals(expected, outputStream.toString());
    }

    @Test
    @DisplayName("Test multiples of 5")
    void testMultiplesOfFive() {
        fizzBuzz.fizzBuzz(10);
        String expected =
                "1\n" +
                        "2\n" +
                        "Fizz\n" +
                        "4\n" +
                        "Buzz\n" +
                        "Fizz\n" +
                        "7\n" +
                        "8\n" +
                        "Fizz\n" +
                        "Buzz\n";

        assertEquals(expected, outputStream.toString());
    }

    @Test
    @DisplayName("Test FizzBuzz numbers")
    void testFizzBuzzNumbers() {
        fizzBuzz.fizzBuzz(30); // Validating multiple FizzBuzz groups
        String expected =
                "1\n2\nFizz\n4\nBuzz\nFizz\n7\n8\nFizz\nBuzz\n11\nFizz\n13\n14\nFizzBuzz\n" +
                        "16\n17\nFizz\n19\nBuzz\nFizz\n22\n23\nFizz\nBuzz\n26\nFizz\n28\n29\nFizzBuzz\n";

        assertEquals(expected, outputStream.toString());
    }

    @Test
    @DisplayName("Edge case: Test negative numbers")
    void testNegativeNumbers() {
        fizzBuzz.fizzBuzz(-1); // Undefined, but should output nothing
        String expected = ""; // Expect no output for invalid range
        assertEquals(expected, outputStream.toString());
    }

    @Test
    @DisplayName("Edge case: Test zero")
    void testZero() {
        fizzBuzz.fizzBuzz(0); // Likely invalid, but should output nothing
        String expected = ""; // Define how you want to handle zero
        assertEquals(expected, outputStream.toString());
    }
}