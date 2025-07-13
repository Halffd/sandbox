package com.half.javalearning.threads.empresa;

import java.util.ArrayList;
import java.util.Random;
import java.util.concurrent.Semaphore;

public class Empresa {
    private final Semaphore pool_fita;
    private final PoolProdutos pool_produtos;
    private final ArrayList<Equipe> turno;
    private final int max_prod_empacotar;
    private final int pool_empacotadores;
    private final int nr_max_equipes;
    private int prod_empacotados;

    public Empresa(int nr_fitas, int pool_empacotadores, int nr_max_equipes, int max_prod_empacotar) throws InterruptedException {
        if ((nr_fitas < 1) || (pool_empacotadores < 2) || (nr_max_equipes < 1) || (max_prod_empacotar < 1)) {
            throw new IllegalArgumentException("Argumentos ilegais utilizados no construtor de Empresa.");
        }

        this.pool_fita = new Semaphore(nr_fitas);
        this.pool_empacotadores = pool_empacotadores;
        this.nr_max_equipes = nr_max_equipes;
        this.max_prod_empacotar = max_prod_empacotar;
        this.pool_produtos = new PoolProdutos(max_prod_empacotar);
        this.turno = new ArrayList<>();
        this.prod_empacotados = 0;

        criarEquipes(nr_fitas);
        turno.forEach(Thread::start);

        for (Equipe eqp : turno) {
            try {
                eqp.join();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }

        for (Equipe eqp : turno) {
            prod_empacotados += eqp.getEmpacotamentos();
        }

        System.out.println("TOTAL DE EMPACOTAMENTOS: " + prod_empacotados);
    }

    private void criarEquipes(int nr_fitas) {
        int empacotadores_disponiveis = pool_empacotadores;
        int i = 1;

        // Create teams with fixed size to avoid deadlock potential
        int tamanho_equipe = Math.max(2, Math.min(nr_fitas, empacotadores_disponiveis / nr_max_equipes));

        while ((i <= nr_max_equipes) && (empacotadores_disponiveis >= 2)) {
            int nr_emp_eqp = Math.min(tamanho_equipe, empacotadores_disponiveis);
            nr_emp_eqp = Math.min(nr_emp_eqp, nr_fitas); // Prevent deadlock

            Equipe eqp = new Equipe("Eqp[" + i + "]", nr_emp_eqp, pool_fita, pool_produtos);
            turno.add(eqp);
            empacotadores_disponiveis -= nr_emp_eqp;
            i++;
        }

        // Remaining workers go to last team
        if (empacotadores_disponiveis > 0) {
            int nr_emp_final = Math.min(empacotadores_disponiveis, nr_fitas);
            Equipe eqp = new Equipe("Eqp[" + i + "]", nr_emp_final, pool_fita, pool_produtos);
            turno.add(eqp);
        }
    }
}