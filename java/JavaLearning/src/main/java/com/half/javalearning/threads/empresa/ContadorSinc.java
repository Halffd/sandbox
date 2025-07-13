package com.half.javalearning.threads.empresa;

public class ContadorSinc {
    private int contador;
    private final int inicio;

    public ContadorSinc(int inicio) {
        this.inicio = inicio;
        this.contador = inicio;
    }

    public synchronized void decrementar() {
        if (contador > 0) {
            contador--;
        }
    }

    public synchronized void incrementar() {
        contador++;
    }

    public synchronized int getContador() {
        return contador;
    }

    public synchronized void resetar() {
        contador = inicio;
    }
}