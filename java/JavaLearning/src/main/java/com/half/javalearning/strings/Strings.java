/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package com.half.javalearning.strings;

import java.util.Scanner;
import java.util.regex.Pattern;

/**
 *
 * @author half
 */
public class Strings {
    public void equal(){
        String a = "hello";
        //String b = "hello";
        //Sy<stem.out.println(a == b); // Might print true due to string pooling

        String c = new Scanner(System.in).nextLine(); // User types "hello"
        System.out.println(a == c); // Will print false even if c contains "hello"
        if(c.toLowerCase().equals(a)){
            System.out.println("Hey");
        }
    }
    public static void main(String[] args) {
        String name = "Programming Coder";

        int length = name.length();
        char letter = name.charAt(0);
        int index = name.indexOf(" ");
        int lastIndex = name.lastIndexOf("o");
        System.out.println("Length: " + length);
        System.out.println("First letter: " + letter);
        System.out.println("Index of ' ': " + index);
        System.out.println("Last index of 'o': " + lastIndex);
        name = name.toUpperCase();
        name = name.toLowerCase();
        name = name.trim();
        name = name.replace("o", "a");

        if(name.isEmpty()){
            System.out.println("Your name is empty");
        }
        else{
            System.out.println("Hello " + name);
        }

        if(name.contains(" ")){
            System.out.println("Your name contains a space");
        }
        else{
            System.out.println("Your name DOESN'T contain any spaces");
        }

        if(name.equalsIgnoreCase("password")){
            System.out.println("Your name can't be password");
        }
        else{
            System.out.println("Hello " + name);
        }
        Strings s = new Strings();
        s.equal();
        System.out.println("Hello, what is your name?");
        String name2 = new Scanner(System.in).nextLine();
        System.out.println("What is your age?");
        int age = new Scanner(System.in).nextInt();
        System.out.println("What is your favorite GPA?");
        double gpa = new Scanner(System.in).nextDouble();
        System.out.println("Enter each of your grades followdn by spaces");
        String[] grades = new Scanner(System.in).nextLine().split(" ");
        System.out.println("Enter dates of your classes, format MM/DD/YYYY, followed by spaces:");

        Scanner scanner = new Scanner(System.in);
        String input = scanner.nextLine();

        // Split input by spaces
        String[] dates = input.split(" ");

        // Regex pattern for MM/DD/YYYY
        String datePattern = "^(0[1-9]|1[0-2])/(0[1-9]|[12][0-9]|3[01])/(\\d{4})$";
        Pattern pattern = Pattern.compile(datePattern);

        System.out.println("Name: " + name2);
        System.out.println("Age: " + age);
        System.out.println("GPA: " + gpa);
        for (String grade : grades) {
            System.out.println(grade);
        }
        // Print and validate each date
        for (String date : dates) {
            if (pattern.matcher(date).matches()) {
                System.out.println(date);
            } else {
                System.out.println("Invalid date format: " + date);
            }
        }
        scanner.close();
    }
}
