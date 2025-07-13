package com.half.javalearning.classes.interfaces;

import java.util.Calendar;

interface Identificador {
    int TAMANHO_MAX = 21; // Constants should be UPPER_CASE
    void validarID(String id);
    void formatarID(int tipo);
    void atualizarID(String id);
    String recuperarID();
}

interface IPessoa {
    void atualizarNome(String nome);
    String recuperarNome();
    String recuperarNacionalidade();
    String recuperarNaturalidade();
    void atualizarIdade(Calendar dataInicioExistencia);
    int recuperarIdade();
    int retornaTipo();
    int calcularIdade(Calendar dataInicioExistencia);
}

class Pessoa implements IPessoa, Identificador {
    // Attributes
    private int idade;
    private String nome = "";
    private String naturalidade = "";
    private String nacionalidade = "";
    private String identificador = "";

    // Constructors
    public Pessoa() {}

    public Pessoa(String nome, String naturalidade, String nacionalidade) {
        this.nome = nome;
        this.naturalidade = naturalidade;
        this.nacionalidade = nacionalidade;
    }

    // IPessoa methods
    @Override
    public void atualizarNome(String nome) {
        if (nome != null && !nome.isBlank()) {
            this.nome = nome;
        } else {
            System.out.println("ERRO: nome em branco ou nulo!");
        }
    }

    @Override
    public String recuperarNome() {
        return this.nome;
    }

    @Override
    public String recuperarNacionalidade() {
        return this.nacionalidade;
    }

    @Override
    public String recuperarNaturalidade() {
        return this.naturalidade;
    }

    @Override
    public void atualizarIdade(Calendar dataInicioExistencia) {
        this.idade = calcularIdade(dataInicioExistencia);
    }

    @Override
    public int recuperarIdade() {
        return this.idade;
    }

    @Override
    public int retornaTipo() {
        return 0; // Whatever this is supposed to represent
    }

    @Override
    public int calcularIdade(Calendar dataInicioExistencia) {
        if (dataInicioExistencia == null) {
            return 0;
        }

        Calendar hoje = Calendar.getInstance();
        int lapso = hoje.get(Calendar.YEAR) - dataInicioExistencia.get(Calendar.YEAR);

        if ((dataInicioExistencia.get(Calendar.MONTH) > hoje.get(Calendar.MONTH)) ||
                (dataInicioExistencia.get(Calendar.MONTH) == hoje.get(Calendar.MONTH) &&
                        dataInicioExistencia.get(Calendar.DATE) > hoje.get(Calendar.DATE))) {
            lapso--;
        }

        return Math.max(0, lapso); // Can't have negative age
    }

    // Identificador methods
    @Override
    public void validarID(String id) {
        // Fixed: interface method was void, but implementation returned boolean
        if (id == null || id.isBlank() || id.isEmpty()) {
            throw new IllegalArgumentException("ID inválido: não pode ser nulo ou vazio");
        }
        if (id.length() > TAMANHO_MAX) {
            throw new IllegalArgumentException("ID muito longo: máximo " + TAMANHO_MAX + " caracteres");
        }
    }

    @Override
    public void formatarID(int tipo) {
        this.identificador = String.format("%0" + Math.min(tipo, TAMANHO_MAX) + "d",
                Math.abs(tipo));
    }

    @Override
    public void atualizarID(String identificador) {
        validarID(identificador); // Validate before setting
        this.identificador = identificador;
    }

    @Override
    public String recuperarID() {
        return this.identificador;
    }

    // Utility methods
    public void setNacionalidade(String nacionalidade) {
        this.nacionalidade = nacionalidade != null ? nacionalidade : "";
    }

    public void setNaturalidade(String naturalidade) {
        this.naturalidade = naturalidade != null ? naturalidade : "";
    }

    @Override
    public String toString() {
        return String.format("Pessoa{nome='%s', idade=%d, naturalidade='%s', nacionalidade='%s', id='%s'}",
                nome, idade, naturalidade, nacionalidade, identificador);
    }
}
public class Interface {
    public static void main(String[] args) {
        Pessoa pessoa = new Pessoa();
        pessoa.atualizarNome("Joaquim");
        pessoa.atualizarIdade(Calendar.getInstance());
        pessoa.atualizarID("123456789");
        System.out.println(pessoa);
    }
}