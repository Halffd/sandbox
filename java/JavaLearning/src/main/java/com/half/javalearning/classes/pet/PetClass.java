package com.half.javalearning.classes;

abstract class Pet {
    abstract String speak();
}

class Cat extends Pet {
    String speak() {
        return "Meow!";
    }
}

class Dog extends Pet {
    String speak() {
        return "Woof!";
    }
}

public class PetClass {
    static void letsHear(final Pet pet) {
        System.out.println(pet.speak());
    }
    public static void main(String[] args) {
        letsHear(new Cat());
        letsHear(new Dog());
    }
}