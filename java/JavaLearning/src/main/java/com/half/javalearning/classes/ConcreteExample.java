package com.half.javalearning.classes;

public class ConcreteExample extends SuperComplexExample<Integer, String> {
    int value;
    public ConcreteExample(Integer value, String secondaryValue) {
        super(value, secondaryValue);
        this.value = value;
    }

    @Override
    public void processData() {
        System.out.println("Processing data in concrete implementation");
    }
    public int getValue() {
        return value;
    }
    @Override
    protected void initialize() {
        setName("ConcreteExample");
    }
}
