package com.half.javalearning.person;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

public class Person2 {
    // Attributes with more inclusive design
    private String name;
    private String gender;        // String allows for self-identification
    private double identifier;    // Unique ID
    private float finances;       // Economic status
    private int age;
    private float height;
    private List<String> traits;  // For representing various traits and characteristics

    // Constructor
    public Person2(String name, String gender, float finances,
                  int age, float height, List<String> traits) {
        Random random = new Random();
        this.setName(name);
        this.setGender(gender);
        this.setFinances(finances);
        this.setAge(age);
        this.setHeight(height);
        this.setTraits(traits);
        this.identifier = random.nextDouble();
    }

    // Getters and Setters with private modifiers for encapsulation
    public String getName() {
        return this.name;
    }

    private void setName(String name) {
        this.name = name;
    }

    public String getGender() {
        return this.gender;
    }

    private void setGender(String gender) {
        this.gender = gender;
    }

    public float getFinances() {
        return this.finances;
    }

    private void setFinances(float finances) {
        this.finances = finances;
    }

    public int getAge() {
        return this.age;
    }

    private void setAge(int age) {
        this.age = age;
    }

    public float getHeight() {
        return this.height;
    }

    private void setHeight(float height) {
        this.height = height;
    }

    public List<String> getTraits() {
        return new ArrayList<>(this.traits); // Return a copy to protect encapsulation
    }

    private void setTraits(List<String> traits) {
        this.traits = new ArrayList<>(traits); // Store a copy to protect encapsulation
    }

    public double getIdentifier() {
        return this.identifier;
    }

    public static void main(String[] args) {
        // Example of creating diverse people
        List<String> traits1 = new ArrayList<>();
        traits1.add("Creative");
        traits1.add("Multilingual");

        List<String> traits2 = new ArrayList<>();
        traits2.add("Athletic");
        traits2.add("Visual artist");
        traits2.add("Uses wheelchair");

        // Creating people with diverse characteristics
        Person2 p1 = new Person2("Alex Chen", "Non-binary", 3500.0f, 28, 1.75f, traits1);
        Person2 p2 = new Person2("Maya Johnson", "Woman", 4200.0f, 34, 1.60f, traits2);

        // Display information
        displayPersonInfo(p1);
        displayPersonInfo(p2);
    }

    private static void displayPersonInfo(Person2 p) {
        String formattedHeight = String.format("%.2f", p.getHeight());
        String formattedFinances = String.format("%.2f", p.getFinances());

        System.out.println("Person: " + p.getName() +
                "\n  Gender: " + p.getGender() +
                "\n  Age: " + p.getAge() +
                "\n  Height: " + formattedHeight + "m" +
                "\n  Finances: " + formattedFinances +
                "\n  Traits: " + String.join(", ", p.getTraits()) +
                "\n  ID: " + p.getIdentifier());
        System.out.println();
    }
}
