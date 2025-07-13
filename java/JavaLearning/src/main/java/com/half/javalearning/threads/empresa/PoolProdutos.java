package com.half.javalearning.threads.empresa;

public final class PoolProdutos extends ContadorSinc {

    public PoolProdutos(int qtdade_produtos) {
        super(qtdade_produtos);
        if (qtdade_produtos < 1) {
            throw new IllegalArgumentException("Argumentos ilegais utilizados no construtor de PoolProdutos.");
        }
    }

    public synchronized int retirarProdutos(int nr_produtos) {
        int aux = getContador();
        if ((aux - nr_produtos) >= 0) {
            // Há produtos disponíveis suficientes
            for (int i = 0; i < nr_produtos; i++) {
                decrementar();
            }
            return nr_produtos;
        } else if (aux > 0) {
            // Retira apenas os disponíveis
            int disponiveis = aux;
            for (int i = 0; i < disponiveis; i++) {
                decrementar();
            }
            return disponiveis;
        } else {
            return 0; // Nenhum produto disponível
        }
    }
}