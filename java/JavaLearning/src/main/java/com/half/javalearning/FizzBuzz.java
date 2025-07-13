/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package com.half.javalearning;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintStream;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintStream;

/**
 * FizzBuzz implementation
 */
class FizzBuzz {
    /**
     * Prints the FizzBuzz sequence up to n
     * @param n The upper limit of the sequence
     */
    public void fizzBuzz(int n) {
        for (int i = 1; i <= n; i++) {
            if (i % 15 == 0) {
                System.out.println("FizzBuzz");
            } else if (i % 3 == 0) {
                System.out.println("Fizz");
            } else if (i % 5 == 0) {
                System.out.println("Buzz");
            } else {
                System.out.println(i);
            }
        }
    }
}

class DoFizzBuzz {
    public static void start(String[] args) {
        FizzBuzz fizzBuzz = new FizzBuzz();
        
        System.out.println(AnsiColors.CYAN + "FizzBuzz up to 15:" + AnsiColors.RESET);
        fizzBuzz.fizzBuzz(15);
        
        System.out.println("\n" + AnsiColors.CYAN + "FizzBuzz up to 30:" + AnsiColors.RESET);
        fizzBuzz.fizzBuzz(30);
    }
}