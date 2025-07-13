package com.half.javalearning.classes.polymorphism;

import java.util.UUID;

//Classe base
class Pessoa {
    //Atributos
    private String nome;
    private String nacionalidade;
    private String naturalidade;
    protected String identificador;

    //Construtor
    public Pessoa(String nome, String nacionalidade, String naturalidade) {
        this.nome = nome;
        this.nacionalidade = nacionalidade;
        this.naturalidade = naturalidade;
        this.identificador = "";
    }

    //Métodos
    protected void atualizarNome(String nome) {
        this.nome = nome;
    }

    protected void atualizarID(String identificador) {
        this.identificador = identificador;
    }

    protected String recuperarID() {
        return this.identificador;
    }

    protected String getNome() {
        return this.nome;
    }
}

//Classe Fisica - pessoa física
class Fisica extends Pessoa {

    public Fisica(String nome, String nacionalidade, String naturalidade) {
        super(nome, nacionalidade, naturalidade);
    }

    @Override
    protected void atualizarID(String CPF) {
        if (validaCPF(CPF)) {
            this.identificador = CPF;
        } else {
            System.out.println("ERRO: CPF invalido!");
        }
    }

    private boolean validaCPF(String cpf) {
        // Validação básica de CPF (simplificada)
        if (cpf == null || cpf.length() != 11) {
            return false;
        }
        // Aqui seria a validação completa do CPF
        return cpf.matches("\\d{11}");
    }
}

//Classe Juridica - pessoa jurídica
class Juridica extends Pessoa {

    public Juridica(String nome, String nacionalidade, String naturalidade) {
        super(nome, nacionalidade, naturalidade);
    }

    @Override
    protected void atualizarID(String CNPJ) {
        if (validaCNPJ(CNPJ)) {
            this.identificador = CNPJ;
        } else {
            System.out.println("ERRO: CNPJ invalido!");
        }
    }

    private boolean validaCNPJ(String cnpj) {
        // Validação básica de CNPJ (simplificada)
        if (cnpj == null || cnpj.length() != 14) {
            return false;
        }
        return cnpj.matches("\\d{14}");
    }
}
//Classe Aluno - especialização de Fisica
class Aluno extends Fisica {
    //Atributos
    private String matricula;

    //Construtor
    public Aluno(String nome, String nacionalidade, String naturalidade) {
        super(nome, nacionalidade, naturalidade);
        this.matricula = UUID.randomUUID().toString();
        // Auto-generate ID on creation
        atualizarIDAluno();
    }

    // Remove @Override since this is a new method, not overriding
    public void atualizarIDAluno() {
        if (this.identificador == null || this.identificador.isBlank()) {
            this.identificador = UUID.randomUUID().toString();
        } else {
            System.out.println("ERRO: Codigo matricula ja existente!");
        }
    }

    // Or if you want to override the parent method:
    @Override
    protected void atualizarID(String id) {
        // Students get auto-generated IDs, ignore the parameter
        atualizarIDAluno();
    }

    public String getMatricula() {
        return this.matricula;
    }
}

//Classe de teste
public class TesteHeranca {
    public static void main(String[] args) {
        // Testando a hierarquia
        Aluno aluno = new Aluno("João Silva", "Brasileira", "São Paulo");
        System.out.println("Nome: " + aluno.getNome());
        System.out.println("Matrícula: " + aluno.getMatricula());

        Fisica pessoa = new Fisica("Maria Santos", "Brasileira", "Rio de Janeiro");
        pessoa.atualizarID("12345678901");
        System.out.println("CPF: " + pessoa.recuperarID());

        Juridica empresa = new Juridica("Tech Corp", "Brasileira", "São Paulo");
        empresa.atualizarID("12345678000195");
        System.out.println("CNPJ: " + empresa.recuperarID());
    }
}
