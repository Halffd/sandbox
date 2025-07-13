package com.half.javalearning.classes.polymorphism;

/**
 * Casting.java - Demonstrates explicit upcasting and downcasting in Java.
 *
 * This code shows both safe and unsafe casting practices, with proper
 * error handling and explanations of when each type of casting occurs.
 */

/**
 * Base class for casting demonstration.
 */
class Base {
    protected int baseVariable;

    public Base() {
        this.baseVariable = -1;
        System.out.println("Base constructor called, baseVariable = " + baseVariable);
    }

    protected void updateBaseVariable(int value) {
        this.baseVariable = value;
        System.out.println("Base: baseVariable updated to " + this.baseVariable);
    }

    protected void printBaseVariable() {
        System.out.println("Base: baseVariable value is " + this.baseVariable);
    }

    /**
     * DANGEROUS DOWNCASTING METHOD - demonstrates the original problematic approach
     * This method assumes 'this' is always a Derived instance
     */
    protected void updateDerivedVariableUnsafe(int value) {
        try {
            // EXPLICIT DOWNCASTING - this is what the original code was doing
            Derived derivedThis = (Derived) this;  // <- DOWNCASTING HAPPENS HERE
            derivedThis.derivedVariable = value;
            System.out.println("Base (via downcasting): derivedVariable updated to " + value);
        } catch (ClassCastException e) {
            System.err.println("CASTING ERROR: Cannot downcast to Derived - " + e.getMessage());
        }
    }

    /**
     * DANGEROUS DOWNCASTING METHOD - prints derived variable via casting
     */
    protected void printDerivedVariableUnsafe() {
        try {
            // EXPLICIT DOWNCASTING AGAIN
            Derived derivedThis = (Derived) this;  // <- DOWNCASTING HAPPENS HERE
            System.out.println("Base (via downcasting): derivedVariable value is " + derivedThis.derivedVariable);
        } catch (ClassCastException e) {
            System.err.println("CASTING ERROR: Cannot downcast to Derived - " + e.getMessage());
        }
    }

    /**
     * SAFE DOWNCASTING - uses instanceof check first
     */
    protected void safeDerivedOperation(int value) {
        if (this instanceof Derived) {  // Check type first
            Derived derivedThis = (Derived) this;  // Safe downcast
            derivedThis.derivedVariable = value;
            System.out.println("Base (safe downcasting): derivedVariable updated to " + value);
        } else {
            System.out.println("Base: This is not a Derived instance, cannot perform derived operation");
        }
    }

    public String getClassName() {
        return "Base";
    }
}

/**
 * Derived class extending Base.
 */
class Derived extends Base {
    protected int derivedVariable;

    public Derived() {
        super(); // Calls Base constructor first
        System.out.println("Derived constructor called, baseVariable before initialization = " + this.baseVariable);

        // Initialize both variables
        this.baseVariable = -2;
        this.derivedVariable = -2;

        System.out.println("Derived: Both variables initialized to -2");
    }

    public void updateDerivedVariable(int value) {
        this.derivedVariable = value;
        System.out.println("Derived: derivedVariable updated to " + this.derivedVariable);
    }

    public void printDerivedVariable() {
        System.out.println("Derived: derivedVariable value is " + this.derivedVariable);
    }

    @Override
    public String getClassName() {
        return "Derived";
    }

    // Method specific to Derived class
    public void derivedOnlyMethod() {
        System.out.println("Derived: This method only exists in Derived class");
    }
}

/**
 * Another derived class for casting demonstration.
 */
class AnotherDerived extends Base {
    protected String specialField;

    public AnotherDerived() {
        super();
        this.specialField = "AnotherDerived";
        System.out.println("AnotherDerived constructor called");
    }

    @Override
    public String getClassName() {
        return "AnotherDerived";
    }

    public void anotherDerivedMethod() {
        System.out.println("AnotherDerived: Special method with field = " + specialField);
    }
}

/**
 * Main class demonstrating various casting scenarios.
 */

public class Casting {

    private static Derived derivedReference;
    private static Base baseReference;
    private static Base[] objectArray;

    public static void main(String[] args) {
        System.out.println("=== JAVA CASTING DEMONSTRATION ===\n");

        // 1. Direct instantiation and method calls
        demonstrateDirectInstantiation();

        // 2. Upcasting demonstration
        demonstrateUpcasting();

        // 3. Downcasting demonstration
        demonstrateDowncasting();

        // 4. Polymorphic array with casting
        demonstratePolymorphicCasting();

        // 5. Dangerous casting scenarios
        demonstrateDangerousCasting();
    }

    /**
     * Demonstrates direct instantiation without casting.
     */
    private static void demonstrateDirectInstantiation() {
        System.out.println("=== 1. DIRECT INSTANTIATION ===");

        derivedReference = new Derived();

        System.out.println("=> Printing base variable:");
        derivedReference.printBaseVariable();

        System.out.println("=> Updating derived variable via unsafe downcasting (derivedVariable = 1000):");
        derivedReference.updateDerivedVariableUnsafe(1000);

        System.out.println("=> Printing derived variable via unsafe downcasting:");
        derivedReference.printDerivedVariableUnsafe();

        System.out.println("=> Printing derived variable directly:");
        derivedReference.printDerivedVariable();

        System.out.println();
    }

    /**
     * Demonstrates UPCASTING - Derived to Base reference.
     */
    private static void demonstrateUpcasting() {
        System.out.println("=== 2. UPCASTING DEMONSTRATION ===");

        Derived derived = new Derived();

        // UPCASTING - implicit, always safe
        Base upcastedReference = derived;  // <- UPCASTING HAPPENS HERE (implicit)
        // Explicit upcasting (same result): Base upcastedReference = (Base) derived;

        System.out.println("Original reference type: " + derived.getClass().getSimpleName());
        System.out.println("Upcast reference type: " + upcastedReference.getClass().getSimpleName());
        System.out.println("Upcast reference getClassName(): " + upcastedReference.getClassName());

        // Can call Base methods through upcast reference
        upcastedReference.updateBaseVariable(100);
        upcastedReference.printBaseVariable();

        // Can call overridden methods (polymorphism)
        System.out.println("Polymorphic call: " + upcastedReference.getClassName());

        // CANNOT call Derived-specific methods without downcasting
        // upcastedReference.derivedOnlyMethod(); // <- This would cause compile error

        System.out.println();
    }

    /**
     * Demonstrates DOWNCASTING - Base to Derived reference.
     */
    private static void demonstrateDowncasting() {
        System.out.println("=== 3. DOWNCASTING DEMONSTRATION ===");

        // Create Derived object but store in Base reference (upcasting)
        Base baseRef = new Derived();  // Upcasting happens here

        System.out.println("Base reference pointing to: " + baseRef.getClass().getSimpleName());

        // SAFE DOWNCASTING with instanceof check
        if (baseRef instanceof Derived) {
            System.out.println("Safe downcasting check passed");

            // EXPLICIT DOWNCASTING
            Derived downcastedRef = (Derived) baseRef;  // <- DOWNCASTING HAPPENS HERE

            System.out.println("Downcast successful!");
            System.out.println("Can now call Derived methods:");

            downcastedRef.updateDerivedVariable(500);
            downcastedRef.derivedOnlyMethod();
        } else {
            System.out.println("Downcasting check failed - not a Derived instance");
        }

        System.out.println();
    }

    /**
     * Demonstrates casting in polymorphic arrays.
     */
    private static void demonstratePolymorphicCasting() {
        System.out.println("=== 4. POLYMORPHIC ARRAY CASTING ===");

        // Create array of Base references containing different object types
        objectArray = new Base[3];
        objectArray[0] = new Base();           // Base object
        objectArray[1] = new Derived();       // Derived object (upcast to Base)
        objectArray[2] = new AnotherDerived(); // AnotherDerived object (upcast to Base)

        // Process each object with appropriate casting
        for (int i = 0; i < objectArray.length; i++) {
            Base currentObject = objectArray[i];
            System.out.println("Processing object " + i + " (actual type: " +
                    currentObject.getClass().getSimpleName() + ")");

            // Safe downcasting with instanceof
            if (currentObject instanceof Derived) {
                System.out.println("  -> Downcasting to Derived");
                Derived derived = (Derived) currentObject;  // DOWNCASTING
                derived.derivedOnlyMethod();

            } else if (currentObject instanceof AnotherDerived) {
                System.out.println("  -> Downcasting to AnotherDerived");
                AnotherDerived anotherDerived = (AnotherDerived) currentObject;  // DOWNCASTING
                anotherDerived.anotherDerivedMethod();

            } else {
                System.out.println("  -> Pure Base object, no downcasting needed");
            }

            // Always safe to call Base methods (polymorphism)
            currentObject.printBaseVariable();
            System.out.println();
        }
    }

    /**
     * Demonstrates dangerous casting scenarios and how they fail.
     */
    private static void demonstrateDangerousCasting() {
        System.out.println("=== 5. DANGEROUS CASTING SCENARIOS ===");

        // Scenario 1: Try to downcast Base object to Derived
        System.out.println("Scenario 1: Downcasting pure Base object to Derived");
        Base pureBase = new Base();

        try {
            // This will throw ClassCastException
            Derived wrongCast = (Derived) pureBase;  // DANGEROUS DOWNCASTING
            wrongCast.derivedOnlyMethod();
        } catch (ClassCastException e) {
            System.err.println("ERROR: " + e.getMessage());
        }

        // Scenario 2: Cross-casting between sibling classes
        System.out.println("\nScenario 2: Cross-casting between sibling classes");
        Base anotherDerived = new AnotherDerived();

        try {
            // This will also throw ClassCastException
            Derived wrongCast = (Derived) anotherDerived;  // DANGEROUS DOWNCASTING
            wrongCast.derivedOnlyMethod();
        } catch (ClassCastException e) {
            System.err.println("ERROR: " + e.getMessage());
        }

        // Scenario 3: Demonstrate the original code's dangerous approach
        System.out.println("\nScenario 3: Original code's dangerous downcasting approach");

        Base pureBase2 = new Base();
        System.out.println("Calling dangerous method on pure Base object:");
        pureBase2.updateDerivedVariableUnsafe(999);  // This will catch ClassCastException

        Base derivedAsBase = new Derived();
        System.out.println("Calling dangerous method on Derived object via Base reference:");
        derivedAsBase.updateDerivedVariableUnsafe(999);  // This will work but is dangerous

        System.out.println("\nUsing safe approach:");
        pureBase2.safeDerivedOperation(888);    // Safe approach
        derivedAsBase.safeDerivedOperation(888); // Safe approach
    }
}
