package com.half.javalearning.classes.object;

import java.util.*;

// Base class - because everything inherits from Object anyway
class Animal {
    protected String name;
    protected int age;
    protected String species;

    public Animal(String name, int age, String species) {
        this.name = name;
        this.age = age;
        this.species = species;
    }

    // toString - actually useful this time
    @Override
    public String toString() {
        return String.format("%s{name='%s', age=%d, species='%s'}",
                getClass().getSimpleName(), name, age, species);
    }

    // equals - proper implementation
    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || getClass() != obj.getClass()) return false;

        Animal animal = (Animal) obj;
        return age == animal.age &&
                Objects.equals(name, animal.name) &&
                Objects.equals(species, animal.species);
    }

    // hashCode - consistent with equals
    @Override
    public int hashCode() {
        return Objects.hash(name, age, species);
    }

    // clone - because why not
    @Override
    protected Object clone() throws CloneNotSupportedException {
        return super.clone();
    }

    public void makeSound() {
        System.out.println(name + " makes some generic animal noise");
    }
}

// Subclass 1 - Dogs being dogs
class Dog extends Animal {
    private String breed;
    private boolean isGoodBoy;

    public Dog(String name, int age, String breed, boolean isGoodBoy) {
        super(name, age, "Canis lupus");
        this.breed = breed;
        this.isGoodBoy = isGoodBoy;
    }

    @Override
    public String toString() {
        return String.format("Dog{name='%s', age=%d, breed='%s', isGoodBoy=%s}",
                name, age, breed, isGoodBoy ? "YES" : "debatable");
    }

    @Override
    public boolean equals(Object obj) {
        if (!super.equals(obj)) return false;
        if (!(obj instanceof Dog)) return false;

        Dog dog = (Dog) obj;
        return isGoodBoy == dog.isGoodBoy &&
                Objects.equals(breed, dog.breed);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), breed, isGoodBoy);
    }

    @Override
    public void makeSound() {
        System.out.println(name + " barks: WOOF! (translation: I am good boy)");
    }
}

// Subclass 2 - Cats being assholes
class Cat extends Animal {
    private int livesRemaining;
    private double attitudeLevel;

    public Cat(String name, int age, int livesRemaining, double attitudeLevel) {
        super(name, age, "Felis catus");
        this.livesRemaining = Math.min(9, Math.max(0, livesRemaining));
        this.attitudeLevel = attitudeLevel;
    }

    @Override
    public String toString() {
        return String.format("Cat{name='%s', age=%d, lives=%d, attitude=%.1f/10}",
                name, age, livesRemaining, attitudeLevel);
    }

    @Override
    public boolean equals(Object obj) {
        if (!super.equals(obj)) return false;
        if (!(obj instanceof Cat)) return false;

        Cat cat = (Cat) obj;
        return livesRemaining == cat.livesRemaining &&
                Double.compare(cat.attitudeLevel, attitudeLevel) == 0;
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), livesRemaining, attitudeLevel);
    }

    @Override
    public void makeSound() {
        System.out.println(name + " meows: *judges you silently*");
    }
}

// Subclass 3 - Birds being weird
class Bird extends Animal {
    private boolean canFly;
    private String favoritePerch;

    public Bird(String name, int age, String species, boolean canFly, String favoritePerch) {
        super(name, age, species);
        this.canFly = canFly;
        this.favoritePerch = favoritePerch;
    }

    @Override
    public String toString() {
        return String.format("Bird{name='%s', age=%d, species='%s', canFly=%s, perch='%s'}",
                name, age, species, canFly, favoritePerch);
    }

    @Override
    public boolean equals(Object obj) {
        if (!super.equals(obj)) return false;
        if (!(obj instanceof Bird)) return false;

        Bird bird = (Bird) obj;
        return canFly == bird.canFly &&
                Objects.equals(favoritePerch, bird.favoritePerch);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), canFly, favoritePerch);
    }

    @Override
    public void makeSound() {
        System.out.println(name + " chirps: *existential bird screaming*");
    }
}

// The main event - where the magic happens
public class ObjectMethodsDemo {
    public static void main(String[] args) {
        // Mixed array of animals - polymorphism in action
        Animal[] zoo = {
                new Dog("Rex", 5, "German Shepherd", true),
                new Cat("Whiskers", 3, 7, 9.5),
                new Bird("Polly", 2, "African Grey", true, "shoulder of doom"),
                new Dog("Buddy", 8, "Golden Retriever", true),
                new Cat("Shadow", 6, 4, 8.7),
                new Animal("Mystery", 1, "Unknown"), // Base class instance
                new Dog("Rex", 5, "German Shepherd", true), // Duplicate for testing
                new Bird("Charlie", 4, "Penguin", false, "iceberg")
        };

        System.out.println("🎪 WELCOME TO THE OBJECT METHODS CIRCUS 🎪\n");

        // toString() demonstration
        System.out.println("📋 ROLL CALL (toString in action):");
        for (int i = 0; i < zoo.length; i++) {
            System.out.printf("[%d] %s\n", i, zoo[i].toString());
        }

        // equals() demonstration
        System.out.println("\n🔍 FINDING DUPLICATES (equals in action):");
        for (int i = 0; i < zoo.length; i++) {
            for (int j = i + 1; j < zoo.length; j++) {
                if (zoo[i].equals(zoo[j])) {
                    System.out.printf("MATCH FOUND! [%d] equals [%d]\n", i, j);
                    System.out.printf("  %s\n", zoo[i]);
                    System.out.printf("  %s\n", zoo[j]);
                }
            }
        }

        // hashCode() demonstration
        System.out.println("\n#️⃣ HASH CODES (for the nerds):");
        Map<Integer, List<Integer>> hashGroups = new HashMap<>();
        for (int i = 0; i < zoo.length; i++) {
            int hash = zoo[i].hashCode();
            hashGroups.computeIfAbsent(hash, k -> new ArrayList<>()).add(i);
            System.out.printf("[%d] %s -> Hash: %d\n", i, zoo[i].getClass().getSimpleName(), hash);
        }

        // Show hash collisions (if any)
        System.out.println("\n💥 HASH COLLISIONS:");
        boolean foundCollisions = false;
        for (Map.Entry<Integer, List<Integer>> entry : hashGroups.entrySet()) {
            if (entry.getValue().size() > 1) {
                System.out.printf("Hash %d: indices %s\n", entry.getKey(), entry.getValue());
                foundCollisions = true;
            }
        }
        if (!foundCollisions) {
            System.out.println("No hash collisions found (boring but good!)");
        }

        // getClass() and instanceof demonstrations
        System.out.println("\n🏷️ CLASS HIERARCHY ANALYSIS:");
        Map<String, Integer> classCounts = new HashMap<>();
        for (Animal animal : zoo) {
            String className = animal.getClass().getSimpleName();
            classCounts.put(className, classCounts.getOrDefault(className, 0) + 1);

            // instanceof checks
            System.out.printf("%s: ", animal.name);
            if (animal instanceof Dog) System.out.print("🐕 ");
            if (animal instanceof Cat) System.out.print("🐱 ");
            if (animal instanceof Bird) System.out.print("🐦 ");
            System.out.printf("(%s)\n", className);
        }

        System.out.println("\n📊 CLASS DISTRIBUTION:");
        classCounts.forEach((className, count) ->
                System.out.printf("%s: %d instance%s\n", className, count, count != 1 ? "s" : ""));

        // Polymorphism in action
        System.out.println("\n🎵 ANIMAL SOUNDS (polymorphism showcase):");
        for (Animal animal : zoo) {
            animal.makeSound();
        }

        // Object methods from Object class
        System.out.println("\n⚙️ RAW OBJECT METHODS:");
        Animal testAnimal = zoo[0];
        System.out.println("getClass(): " + testAnimal.getClass());
        System.out.println("getClass().getName(): " + testAnimal.getClass().getName());
        System.out.println("getClass().getSuperclass(): " + testAnimal.getClass().getSuperclass());

        // finalize() is deprecated but let's mention it exists
        System.out.println("\n💀 DEPRECATED/RARELY USED METHODS:");
        System.out.println("finalize() - deprecated since Java 9, don't use it");
        System.out.println("wait(), notify(), notifyAll() - for thread synchronization");

        System.out.println("\n🎉 END OF DEMO - That's how you actually do Object methods!");
    }
}