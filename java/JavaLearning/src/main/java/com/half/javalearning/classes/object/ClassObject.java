package com.half.javalearning.classes.object;

/**
 * ObjectAndClassDemo.java - Demonstrates the Object class and Class reflection
 * in the context of inheritance, polymorphism, and casting.
 *
 * Shows how every class inherits from Object and how to use Class objects
 * for runtime type information and safe casting.
 */

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.Arrays;

/**
 * Base class that explicitly shows Object inheritance.
 * Every class implicitly extends Object, but we'll be explicit here.
 */
class Vehicle extends Object {  // Explicit Object inheritance (usually implicit)

    protected String type;
    protected int year;

    public Vehicle(String type, int year) {
        super(); // Calls Object constructor (usually implicit)
        this.type = type;
        this.year = year;
    }

    // Override Object methods to show polymorphism
    @Override
    public String toString() {
        return "Vehicle{type='" + type + "', year=" + year + "}";
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || getClass() != obj.getClass()) return false;

        Vehicle vehicle = (Vehicle) obj;
        return year == vehicle.year &&
                (type != null ? type.equals(vehicle.type) : vehicle.type == null);
    }

    @Override
    public int hashCode() {
        int result = type != null ? type.hashCode() : 0;
        result = 31 * result + year;
        return result;
    }

    public void startEngine() {
        System.out.println("Vehicle engine started");
    }
}

/**
 * Car class extending Vehicle.
 */
class Car extends Vehicle {

    private String model;
    private int doors;

    public Car(String model, int year, int doors) {
        super("Car", year);
        this.model = model;
        this.doors = doors;
    }

    @Override
    public String toString() {
        return "Car{model='" + model + "', year=" + year + ", doors=" + doors + "}";
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || getClass() != obj.getClass()) return false;
        if (!super.equals(obj)) return false;

        Car car = (Car) obj;
        return doors == car.doors &&
                (model != null ? model.equals(car.model) : car.model == null);
    }

    @Override
    public int hashCode() {
        int result = super.hashCode();
        result = 31 * result + (model != null ? model.hashCode() : 0);
        result = 31 * result + doors;
        return result;
    }

    @Override
    public void startEngine() {
        System.out.println("Car engine started with key");
    }

    public void honkHorn() {
        System.out.println("Car horn: BEEP BEEP!");
    }
}

/**
 * Truck class extending Vehicle.
 */
class Truck extends Vehicle {

    private double loadCapacity;

    public Truck(int year, double loadCapacity) {
        super("Truck", year);
        this.loadCapacity = loadCapacity;
    }

    @Override
    public String toString() {
        return "Truck{year=" + year + ", loadCapacity=" + loadCapacity + " tons}";
    }

    @Override
    public void startEngine() {
        System.out.println("Truck diesel engine started");
    }

    public void loadCargo() {
        System.out.println("Loading cargo into truck");
    }
}

/**
 * Main demonstration class.
 */
public class ClassObject {

    public static void main(String[] args) {
        System.out.println("=== OBJECT CLASS AND CLASS REFLECTION DEMO ===\n");

        // 1. Demonstrate Object methods inheritance
        demonstrateObjectMethods();

        // 2. Demonstrate Class objects and getClass()
        demonstrateClassObjects();

        // 3. Demonstrate runtime type checking
        demonstrateRuntimeTypeChecking();

        // 4. Demonstrate safe casting using Class objects
        demonstrateSafeCastingWithClass();

        // 5. Demonstrate reflection capabilities
        demonstrateReflection();

        // 6. Demonstrate polymorphic behavior with Object
        demonstratePolymorphismWithObject();
    }

    /**
     * Shows how all objects inherit Object methods.
     */
    private static void demonstrateObjectMethods() {
        System.out.println("=== 1. OBJECT METHODS INHERITANCE ===");

        Car car1 = new Car("Honda Civic", 2020, 4);
        Car car2 = new Car("Honda Civic", 2020, 4);
        Car car3 = new Car("Toyota Camry", 2021, 4);

        // Every object has these methods from Object class
        System.out.println("car1.toString(): " + car1.toString());
        System.out.println("car1.hashCode(): " + car1.hashCode());
        System.out.println("car1.equals(car2): " + car1.equals(car2)); // Should be true
        System.out.println("car1.equals(car3): " + car1.equals(car3)); // Should be false

        // getClass() returns the Class object
        System.out.println("car1.getClass(): " + car1.getClass());
        System.out.println("car1.getClass().getName(): " + car1.getClass().getName());

        System.out.println();
    }

    /**
     * Demonstrates Class objects and their properties.
     */
    private static void demonstrateClassObjects() {
        System.out.println("=== 2. CLASS OBJECTS DEMONSTRATION ===");

        Car car = new Car("BMW", 2022, 4);
        Truck truck = new Truck(2021, 5.5);

        // Get Class objects in different ways
        Class<Car> carClass1 = Car.class;                    // Class literal
        Class<?> carClass2 = car.getClass();                 // Runtime class
        Class<?> truckClass = truck.getClass();              // Runtime class

        System.out.println("Car.class: " + carClass1);
        System.out.println("car.getClass(): " + carClass2);
        System.out.println("truck.getClass(): " + truckClass);

        // Class object properties
        System.out.println("\nClass Properties:");
        System.out.println("Car class name: " + carClass1.getName());
        System.out.println("Car simple name: " + carClass1.getSimpleName());
        System.out.println("Car canonical name: " + carClass1.getCanonicalName());
        System.out.println("Car superclass: " + carClass1.getSuperclass());
        System.out.println("Car is interface: " + carClass1.isInterface());
        System.out.println("Car is primitive: " + carClass1.isPrimitive());
        System.out.println("Car is array: " + carClass1.isArray());

        System.out.println();
    }

    /**
     * Demonstrates runtime type checking using Class objects.
     */
    private static void demonstrateRuntimeTypeChecking() {
        System.out.println("=== 3. RUNTIME TYPE CHECKING ===");

        // Create array of Vehicle references (upcasting)
        Vehicle[] vehicles = {
                new Car("Tesla Model 3", 2023, 4),
                new Truck(2022, 8.0),
                new Vehicle("Generic", 2020)
        };

        for (int i = 0; i < vehicles.length; i++) {
            Vehicle vehicle = vehicles[i];

            System.out.println("Vehicle " + i + ":");
            System.out.println("  toString(): " + vehicle.toString());
            System.out.println("  getClass(): " + vehicle.getClass());
            System.out.println("  getClass().getSimpleName(): " + vehicle.getClass().getSimpleName());

            // Different ways to check type
            System.out.println("  instanceof Car: " + (vehicle instanceof Car));
            System.out.println("  instanceof Truck: " + (vehicle instanceof Truck));
            System.out.println("  instanceof Vehicle: " + (vehicle instanceof Vehicle));
            System.out.println("  instanceof Object: " + (vehicle instanceof Object));

            // Using Class objects for type checking
            System.out.println("  getClass() == Car.class: " + (vehicle.getClass() == Car.class));
            System.out.println("  getClass() == Truck.class: " + (vehicle.getClass() == Truck.class));

            System.out.println();
        }
    }

    /**
     * Demonstrates safe casting using Class objects and reflection.
     */
    private static void demonstrateSafeCastingWithClass() {
        System.out.println("=== 4. SAFE CASTING WITH CLASS OBJECTS ===");

        Object[] objects = {
                new Car("Ford Mustang", 2023, 2),
                new Truck(2021, 12.5),
                "This is a String",
                42,
                new Vehicle("Motorcycle", 2022)
        };

        for (Object obj : objects) {
            System.out.println("Processing object: " + obj);
            System.out.println("  Runtime type: " + obj.getClass().getSimpleName());

            // Safe casting using Class.isInstance()
            if (Car.class.isInstance(obj)) {
                Car car = Car.class.cast(obj);  // Safe cast using Class.cast()
                System.out.println("  -> Successfully cast to Car: " + car.toString());
                car.honkHorn();

            } else if (Truck.class.isInstance(obj)) {
                Truck truck = Truck.class.cast(obj);  // Safe cast using Class.cast()
                System.out.println("  -> Successfully cast to Truck: " + truck.toString());
                truck.loadCargo();

            } else if (Vehicle.class.isInstance(obj)) {
                Vehicle vehicle = Vehicle.class.cast(obj);  // Safe cast using Class.cast()
                System.out.println("  -> Successfully cast to Vehicle: " + vehicle.toString());
                vehicle.startEngine();

            } else {
                System.out.println("  -> Not a Vehicle type, it's a " + obj.getClass().getSimpleName());
            }

            System.out.println();
        }
    }

    /**
     * Demonstrates reflection capabilities using Class objects.
     */
    private static void demonstrateReflection() {
        System.out.println("=== 5. REFLECTION CAPABILITIES ===");

        Class<Car> carClass = Car.class;

        System.out.println("Analyzing Car class:");

        // Get all methods (including inherited ones)
        Method[] methods = carClass.getMethods();
        System.out.println("Public methods (" + methods.length + "):");
        for (Method method : methods) {
            String modifiers = Modifier.toString(method.getModifiers());
            String returnType = method.getReturnType().getSimpleName();
            String name = method.getName();
            String params = Arrays.toString(method.getParameterTypes());

            System.out.println("  " + modifiers + " " + returnType + " " + name + params);
        }

        // Get declared methods (only this class)
        Method[] declaredMethods = carClass.getDeclaredMethods();
        System.out.println("\nDeclared methods in Car class only (" + declaredMethods.length + "):");
        for (Method method : declaredMethods) {
            System.out.println("  " + method.getName());
        }

        // Check inheritance hierarchy
        System.out.println("\nInheritance hierarchy:");
        Class<?> currentClass = carClass;
        while (currentClass != null) {
            System.out.println("  " + currentClass.getName());
            currentClass = currentClass.getSuperclass();
        }

        System.out.println();
    }

    /**
     * Demonstrates polymorphism with Object references.
     */
    private static void demonstratePolymorphismWithObject() {
        System.out.println("=== 6. POLYMORPHISM WITH OBJECT REFERENCES ===");

        // Everything is an Object - ultimate upcasting
        Object[] objects = {
                new Car("Audi A4", 2023, 4),
                new Truck(2022, 15.0),
                new Vehicle("Boat", 2021),
                "Hello World",
                new Integer(100),
                new double[]{1.1, 2.2, 3.3}
        };

        System.out.println("Processing array of Object references:");

        for (int i = 0; i < objects.length; i++) {
            Object obj = objects[i];

            System.out.println("Object " + i + ":");

            // These methods work for ALL objects (polymorphism)
            System.out.println("  toString(): " + obj.toString());
            System.out.println("  hashCode(): " + obj.hashCode());
            System.out.println("  getClass(): " + obj.getClass());

            // Type-specific behavior using safe downcasting
            if (obj instanceof Vehicle) {
                Vehicle vehicle = (Vehicle) obj;
                System.out.println("  -> Vehicle behavior:");
                vehicle.startEngine();

                // Further downcasting if needed
                if (obj instanceof Car) {
                    Car car = (Car) obj;
                    car.honkHorn();
                } else if (obj instanceof Truck) {
                    Truck truck = (Truck) obj;
                    truck.loadCargo();
                }
            } else {
                System.out.println("  -> Not a Vehicle: " + obj.getClass().getSimpleName());
            }

            System.out.println();
        }
    }
}
