package com.half.javalearning.aluno;

public class AlunoTeste {
    /* Atributos */
    private String nome;
    /* Getters e Setters */
    public String getNome() {
        return nome;
    }
    public void setNome(String nome) {
        this.nome = nome;
    }
    // Static methods
    public static void criar(){
        AlunoTeste a = new AlunoTeste();
        a.setNome("Joao");
        System.out.println("saida: "+a.getNome());
    }
}