package com.half.javalearning.files;

// Actually sensible custom exception
public class ArrayCopyException extends RuntimeException {
    public ArrayCopyException(String message) {
        super(message);
    }
    
    public ArrayCopyException(String message, Throwable cause) {
        super(message, cause);
    }
}
