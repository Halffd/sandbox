/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package com.half.javalearning.strings;

import java.util.Scanner;

/**
 *
 * @author half
 */

class Palindromes {
    public Palindromes() {
        String s1, s2 = "";
        Scanner input = new Scanner(System.in);

        System.out.print("Enter any string to check if it is a palindrome: ");
        s1 = input.nextLine();
        
        // Remove spaces and punctuation, and convert to lowercase
        s1 = s1.replaceAll("\\s", ""); // Remove spaces
        s1 = s1.replaceAll("\\W", ""); // Remove punctuation
        s1 = s1.toLowerCase();

        int length = s1.length();

        // Reverse the string
        for (int i = length - 1; i >= 0; i--) { 
            s2 += s1.charAt(i);
        }

        // Check if the string is a palindrome
        if (s1.equals(s2)) {
            System.out.println("This string is a palindrome.");
        } else {
            System.out.println("This string is not a palindrome.");
        }

        input.close(); // Close the scanner
    }
}