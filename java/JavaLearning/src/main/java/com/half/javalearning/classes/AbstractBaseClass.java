package com.half.javalearning.classes;

public abstract class AbstractBaseClass {
    protected String name;
    
    public AbstractBaseClass() {
        this.name = "Default";
    }
    
    public String getName() {
        return name;
    }
    
    public void setName(String name) {
        this.name = name;
    }
    
    // Abstract method to be implemented by subclasses
    protected abstract void initialize();
}
