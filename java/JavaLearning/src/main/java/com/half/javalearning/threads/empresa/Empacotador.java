package com.half.javalearning.threads.empresa;

public class Empacotador implements Runnable {
    private final Equipe equipe;
    private final ContadorSinc empacotamentos;
    private final String nome;
    private String lista_threads_id;

    public Empacotador(int nr_empacotador, Equipe equipe) {
        this.equipe = equipe;
        this.lista_threads_id = "";
        this.nome = "Emp[" + nr_empacotador + "]@" + equipe.getName();
        this.empacotamentos = new ContadorSinc(0);
    }

    public void listarIdThreads() {
        System.out.println(" |----- Lista de threads executadas por " + nome + " : " + lista_threads_id);
    }

    public void listarEmpacotamentos() {
        System.out.println(" |----- Empacotamentos feitos por " + nome + " : " + empacotamentos.getContador());
    }

    @Override
    public void run() {
        try {
            synchronized (this) {
                lista_threads_id += "[" + Thread.currentThread().getId() + "]";
            }

            System.out.println(nome + " empacotando (" + System.currentTimeMillis() + ")");
            Thread.sleep((int)(Math.random() * 900 + 100));
            System.out.println(nome + " concluiu  (" + System.currentTimeMillis() + ")");

            empacotamentos.incrementar();
            equipe.liberarFita();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }
}
