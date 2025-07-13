package com.half.javalearning.threads;

import java.util.Random;

public class ThreadInterface implements Runnable {
    long numero;
    ThreadInterface  (long numero) {
        this.numero = numero;
    }

    public void run() {
        // Implementa o comportamento apropriado
        var i = new Random().nextInt(100000000);
        while (i > 0) {
            numero += 1 * i;
            System.out.println("ThreadInterface: " + i);
            System.out.println("ThreadInterface: " + numero);
            i--;
            Thread.yield();
        }
    }
    public static void main(String[] args) {
        ThreadInterface thread = new ThreadInterface(1);
        Thread t = new Thread(thread);
        t.start();
    }
} 
