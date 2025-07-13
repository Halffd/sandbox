package com.half.javalearning;


import java.util.Scanner;

public class PizzaOrder {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);

        System.out.println("Welcome to Pizza Order System!");
        System.out.println("What type of item would you like?");
        System.out.println("1. Pizza");

        int choice = scanner.nextInt();

        if (choice == 1) {
            System.out.println("You've selected Pizza.");
            System.out.println("Please enter your name:");
            scanner.nextLine(); // Consume newline left-over
            String name = scanner.nextLine();

            System.out.println("Please enter your address:");
            String address = scanner.nextLine();

            System.out.println("Please enter your contact number:");
            String contactNumber = scanner.nextLine();

            System.out.println("\nOrder Summary:");
            System.out.println("Name: " + name);
            System.out.println("Address: " + address);
            System.out.println("Contact Number: " + contactNumber);
            System.out.println("Item: Pizza");
            System.out.println("\nThank you for your order! Your Pizza will be delivered soon.");
        } else {
            System.out.println("Invalid selection. Please try again.");
        }

        scanner.close();
    }
}