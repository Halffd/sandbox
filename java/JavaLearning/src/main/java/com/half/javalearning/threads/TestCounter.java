package com.half.javalearning.threads;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Semaphore;

class Contador {
    private volatile int contador = 0;
    private final Semaphore semaforo = new Semaphore(1); // Inicializa o semáforo com 1 permissão

    public void incrementar() {
        try {
            semaforo.acquire(); // Adquire a permissão do semáforo
            contador++; // Incrementa o contador de forma segura
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new RuntimeException("Thread interrompida", e);
        } finally {
            semaforo.release(); // Libera a permissão do semáforo
        }
    }

    public int getContador() {
        return contador;
    }
}

public class TestCounter {
    public static void main(String[] args) {
        Contador contador = new Contador();

        int numThreads = 10;
        int numIncrementos = 1000;

        ExecutorService executor = Executors.newFixedThreadPool(numThreads);

        for(int i = 0; i < numThreads; i++) {
            executor.submit(() -> {  // Fixed: "- >" became "->"
                for(int j = 0; j < numIncrementos; j++) {
                    contador.incrementar();
                }
            });
        }

        executor.shutdown();

        while(!executor.isTerminated()) {  // Fixed: Added parentheses
            // Espera até que todas as threads terminem
            System.out.println("Esperando até"); 
        }

        System.out.println("Valor final do contador: " + contador.getContador());
    }
}