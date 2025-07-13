package com.half.javalearning.classes;

import java.awt.Color;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

// Asset Manager singleton class
class AssetManager {
    private static AssetManager instance;

    private AssetManager() {
        // Private constructor for singleton
    }

    public static AssetManager getInstance() {
        if (instance == null) {
            instance = new AssetManager();
        }
        return instance;
    }

    public Object newObject(String objectType) {
        // In a real implementation, this would be a factory method
        if ("CarFactory".equals(objectType)) {
            return new CarFactory();
        }
        return null;
    }

    public Object newObject(String objectType, String name) {
        if ("Humanoid".equals(objectType)) {
            return new Humanoid(name);
        }
        return null;
    }
}

// Humanoid class for employees
class Humanoid {
    private String name;

    public Humanoid(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }
}
// Main Car class
class Car {
    private UUID carID;
    private String name;
    private String owner;
    private Color originalColor;
    private List<Part> parts;

    // Constructor
    public Car(String name, String owner, Color originalColor) {
        this.carID = UUID.randomUUID(); // Generate unique ID
        this.name = name;
        this.owner = owner;
        this.originalColor = originalColor;
        this.parts = new ArrayList<>();
    }

    // Getters and setters
    public UUID getCarID() {
        return carID;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getOwner() {
        return owner;
    }

    public void setOwner(String owner) {
        this.owner = owner;
    }

    public Color getOriginalColor() {
        return originalColor;
    }

    public void setOriginalColor(Color originalColor) {
        this.originalColor = originalColor;
    }

    // Methods for managing parts
    public void addPart(Part part) {
        parts.add(part);
    }

    public void removePart(Part part) {
        parts.remove(part);
    }

    public List<Part> getParts() {
        return new ArrayList<>(parts); // Return a copy to protect encapsulation
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Car ID: ").append(carID).append("\n");
        sb.append("Name: ").append(name).append("\n");
        sb.append("Owner: ").append(owner).append("\n");
        sb.append("Color: RGB(").append(originalColor.getRed()).append(",")
                .append(originalColor.getGreen()).append(",")
                .append(originalColor.getBlue()).append(")\n");
        sb.append("Parts: ").append(parts.size()).append("\n");

        for (Part part : parts) {
            sb.append("  - ").append(part.toString()).append("\n");
        }

        return sb.toString();
    }
}

// Part class
class Part {
    private String name;
    private Manufacturer manufacturer;
    private String partID;

    public Part(String name, Manufacturer manufacturer, String partID) {
        this.name = name;
        this.manufacturer = manufacturer;
        this.partID = partID;
    }

    // Getters and setters
    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Manufacturer getManufacturer() {
        return manufacturer;
    }

    public void setManufacturer(Manufacturer manufacturer) {
        this.manufacturer = manufacturer;
    }

    public String getPartID() {
        return partID;
    }

    public void setPartID(String partID) {
        this.partID = partID;
    }

    @Override
    public String toString() {
        return name + " (ID: " + partID + ") made by " + manufacturer.getName();
    }
}

// Manufacturer class
class Manufacturer {
    private String name;
    private int manufacturerID;

    public Manufacturer(String name, int manufacturerID) {
        this.name = name;
        this.manufacturerID = manufacturerID;
    }

    // Getters and setters
    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public int getManufacturerID() {
        return manufacturerID;
    }

    public void setManufacturerID(int manufacturerID) {
        this.manufacturerID = manufacturerID;
    }

    @Override
    public String toString() {
        return name + " (ID: " + manufacturerID + ")";
    }
}

// Example usage in a main class
class CarInventoryExample {
    public static void main(String[] args) {
        // Create a car
        Car myCar = new Car("SuperCruiser", "John Doe", new Color(255, 0, 0));

        // Create manufacturers
        Manufacturer bosch = new Manufacturer("Bosch", 1001);
        Manufacturer continental = new Manufacturer("Continental", 1002);

        // Add parts to the car
        myCar.addPart(new Part("Engine", bosch, "E-123"));
        myCar.addPart(new Part("Tires", continental, "T-456"));
        myCar.addPart(new Part("Brakes", bosch, "B-789"));

        // Print car details
        System.out.println(myCar);

        // Demonstrate creating multiple cars and storing them in a collection
        List<Car> carInventory = new ArrayList<>();
        carInventory.add(myCar);
        carInventory.add(new Car("EcoRunner", "Jane Smith", new Color(0, 255, 0)));

        System.out.println("\nCar Inventory contains " + carInventory.size() + " cars");
    }
}

// CarFactory class
class CarFactory {
    private Humanoid employee;

    public void hire(Humanoid employee) {
        this.employee = employee;
        System.out.println("Hired " + employee.getName() + " to build cars!");
    }

    public Car newCar(String model) {
        if (employee == null) {
            System.out.println("No employees to build cars!");
            return null;
        }
        System.out.println(employee.getName() + " is building a " + model + " car");
        return new Car(model, employee.getName(), Color.BLACK);
    }

    // Static factory method for cleaner implementation
    public static Car car(String model) {
        System.out.println("Creating a " + model + " car with static factory method");
        return new Car(model, "John Doe", Color.BLACK);
    }
    public static void main(String[] args) {
        // Overly verbose version
        System.out.println("=== Overly Complicated Way ===");
        AssetManager globalAssetManager = AssetManager.getInstance();
        CarFactory globalCarFactory = (CarFactory) globalAssetManager.newObject("CarFactory");
        globalCarFactory.hire((Humanoid) globalAssetManager.newObject("Humanoid", "Bob"));

        try {
            // This will throw an exception as "car" can't be parsed as an integer
            Car myCar = globalCarFactory.newCar(Integer.toString(Integer.parseInt("car")));
            System.out.println("Created: " + myCar);
        } catch (NumberFormatException e) {
            System.out.println("Error: Cannot convert 'car' to an integer! That was a silly idea anyway.");

            // Let's do it properly instead
            Car myCar = globalCarFactory.newCar("car");
            System.out.println("Created: " + myCar);
        }

        // Cleaner version
        System.out.println("\n=== Simple Way ===");
        Car anotherCar = CarFactory.car("car");
        System.out.println("Created: " + anotherCar);
    }
}
