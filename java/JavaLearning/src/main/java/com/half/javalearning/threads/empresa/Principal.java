package com.half.javalearning.threads.empresa;

// Principal.java
public class Principal {
    private static Empresa ACME;

    public static void main(String args[]) throws InterruptedException {
        ACME = new Empresa(20, 25, 4, 200);
    }
}