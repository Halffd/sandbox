package com.half.javalearning.classes;

import java.io.Serializable;

// @Deprecated
@SuppressWarnings("unchecked")
public abstract class SuperComplexExample<T extends Number, E>
        extends AbstractBaseClass
        implements Runnable, Serializable, Comparable<T> {

    private static final long serialVersionUID = 1L;
    
    // Class body
    private T value;
    private E secondaryValue;

    // Constructor
    public SuperComplexExample(T value, E secondaryValue) {
        this.value = value;
        this.secondaryValue = secondaryValue;
    }

    // Abstract method (must be implemented by non-abstract subclasses)
    public abstract void processData();

    // Static nested class
    public static class InnerHelper {
        public void assist() {
            System.out.println("Helping from inside!");
        }
    }

    // Method implementation from an interface
    @Override
    public int compareTo(T other) {
        return Double.compare(this.value.doubleValue(), other.doubleValue());
    }

    @Override
    public void run() {
        System.out.println("Running with value: " + value);
    }
}
