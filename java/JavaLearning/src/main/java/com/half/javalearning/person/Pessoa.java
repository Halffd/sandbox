package com.half.javalearning.person;

import java.util.Random;
//Classe
public class Pessoa {
    //Atributos
    private String nome;
    private double codigo_identificador;
    private Random aleatorio;
    private float dinheiro;
    private int idade;
    private boolean sexo;
    private float altura;

    //Métodos
    public Pessoa (String nome, float dinheiro, int idade, boolean sexo, float altura) {
        aleatorio = new Random();
        this.setNome(nome);
        this.setDinheiro(dinheiro);
        this.setIdade(idade);
        this.setSexo(sexo);
        this.setAltura(altura);
        this.codigo_identificador = aleatorio.nextDouble();
    }
    public float getDinheiro () {
        return this.dinheiro;
    }
    private void setDinheiro (float dinheiro) {
        this.dinheiro = dinheiro;
    }
    public int getIdade () {
        return this.idade;
    }
    private void setIdade (int idade) {
        this.idade = idade;
    }
    public String getSexo () {
        return this.sexo ? "M" : "F";
    }
    private void setSexo (boolean sexo) {
        this.sexo = sexo;
    }
    public float getAltura () {
        return this.altura;
    }
    private void setAltura (float altura) {
        this.altura = altura;
    }
    private void setNome (String nome) {
        this.nome = nome;
    }
    public String getNome () {
        return this.nome;
    }
    public double getCodigoIdentificador (){
        return this.codigo_identificador;
    }
    public static void main(String args[]){
        Random random = new Random();
        for (int i = 0; i < 10; i++) {

        String nome = Person.generateRandomFullName(3, 8, random);
        float dinheiro = random.nextFloat(10.0f, 10000.0f);
        int idade = random.nextInt(0, 55 );
        boolean sexo = random.nextBoolean();
        float altura = random.nextFloat(0.8f, 2.5f);
        Pessoa p1 = new Pessoa(nome, dinheiro, idade, sexo, altura);
        String height = String.format("%.2f", p1.getAltura());
        String money = String.format("%.2f", p1.getDinheiro());
        System.out.println("Anon's Name:"+p1.getNome() + ", Money:" + money + ", Age:" + p1.getIdade() + ", Gender:" + p1.getSexo() + ", Height:" + height + "m, ID:" + p1.getCodigoIdentificador());
        }
    }
}