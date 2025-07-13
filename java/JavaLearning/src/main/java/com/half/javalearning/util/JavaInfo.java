package com.half.javalearning.util;
public class JavaInfo {
    public static void main(String[] args) {
        System.out.println("Java version: " + System.getProperty("java.version"));
        System.out.println("Java vendor: " + System.getProperty("java.vendor"));
        System.out.println("Java home: " + System.getProperty("java.home"));
        System.out.println("Java class path: " + System.getProperty("java.class.path"));
        System.out.println("Java class version: " + System.getProperty("java.class.version"));
        System.out.println("Java compiler: " + System.getProperty("java.compiler"));
        System.out.println("Java runtime name: " + System.getProperty("java.runtime.name"));
        System.out.println("Java runtime version: " + System.getProperty("java.runtime.version"));
        System.out.println("Java spec name: " + System.getProperty("java.spec.name"));
        System.out.println("Java spec vendor: " + System.getProperty("java.spec.vendor"));
        System.out.println("Java spec version: " + System.getProperty("java.spec.version"));
        System.out.println("Java vm name: " + System.getProperty("java.vm.name"));
        System.out.println("Java vm vendor: " + System.getProperty("java.vm.vendor"));
        System.out.println("Java vm version: " + System.getProperty("java.vm.version"));

        //system properties
        System.out.println("System properties: " + System.getProperties());
        // os properties
        System.out.println("OS name: " + System.getProperty("os.name"));
        System.out.println("OS version: " + System.getProperty("os.version"));
        System.out.println("OS architecture: " + System.getProperty("os.arch"));
        // user properties
        System.out.println("User name: " + System.getProperty("user.name"));
        System.out.println("User home: " + System.getProperty("user.home"));
        System.out.println("User dir: " + System.getProperty("user.dir"));
        // memory properties
        System.out.println("Free memory: " + Runtime.getRuntime().freeMemory());
        System.out.println("Total memory: " + Runtime.getRuntime().totalMemory());
        System.out.println("Max memory: " + Runtime.getRuntime().maxMemory());
        // cpu properties
        System.out.println("Available processors: " + Runtime.getRuntime().availableProcessors());
        // class loader properties
        System.out.println("Class loader: " + ClassLoader.getSystemClassLoader());
        // class path properties
        System.out.println("Class path: " + System.getProperty("java.class.path"));
        // class version properties
        System.out.println("Class version: " + System.getProperty("java.class.version"));
    }
} 