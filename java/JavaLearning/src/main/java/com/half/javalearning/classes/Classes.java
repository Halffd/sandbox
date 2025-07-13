package com.half.javalearning.classes;

// @AutoValue
public abstract class Classes {
    public abstract int getValue();

    public static Builder builder() {
        return null; //new AutoValue_MyClass.Builder();
    }

    // @AutoValue.Builder
    public abstract static class Builder {
        public abstract Builder setValue(int value);
        public abstract Classes build();
    }
}