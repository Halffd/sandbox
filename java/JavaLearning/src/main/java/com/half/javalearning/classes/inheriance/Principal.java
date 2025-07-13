package com.half.javalearning.classes.inheriance;

import java.util.Calendar;

/**
 * Classe que representa um endereço
 */
class Endereco {
    private String nomeRua;
    private int numero;
    private String pais;
    private String uf;
    private String cidade;
    private String numeroStr; // For alphanumeric numbers like "156A"
    private int cep;
    private String complemento;

    // Default constructor (for the Principal class)
    public Endereco() {
        // Initialize with default values
    }

    // Constructor with basic info
    public Endereco(String nomeRua, int numero) {
        this.nomeRua = nomeRua;
        this.numero = numero;
    }

    // Getters
    public String getNomeRua() {
        return nomeRua;
    }

    public int getNumero() {
        return numero;
    }

    public String getPais() {
        return pais;
    }

    public String getUF() {
        return uf;
    }

    public String getCidade() {
        return cidade;
    }

    public String getNumeroStr() {
        return numeroStr;
    }

    public int getCEP() {
        return cep;
    }

    public String getComplemento() {
        return complemento;
    }

    // Setters (the "definir" methods)
    public void definirPais(String pais) {
        this.pais = pais;
    }

    public void definirUF(String uf) {
        this.uf = uf;
    }

    public void definirCidade(String cidade) {
        this.cidade = cidade;
    }

    public void definirRua(String rua) {
        this.nomeRua = rua;
    }

    public void definirNumero(String numero) {
        this.numeroStr = numero;
    }

    public void definirCEP(int cep) {
        this.cep = cep;
    }

    public void definirComplemento(String complemento) {
        this.complemento = complemento;
    }

    // Demo main method
    public static void main(String args[]) {
        Endereco ender = new Endereco("rua X", 7);
        System.out.println(ender.getNomeRua() + ", " + ender.getNumero());
    }
}
class Pessoa {
    //Atributos
    private String nome;
    private int idade;
    private Calendar data_nascimento;
    private long CPF;
    private Endereco endereco;
    //Métodos
    public Pessoa(String nome, Calendar data_nascimento, long CPF, Endereco endereco){
        this.nome = nome;
        this.data_nascimento = data_nascimento;
        this.CPF = CPF;
        this.endereco = endereco;
        atualizarIdade();
    }
    protected void atualizarNome(String nome){
        this.nome = nome;
    }
    protected String recuperarNome(){
        return this.nome;
    }
    protected void atualizarIdade(){
        this.idade = calcularIdade();
    }
    protected int recuperarIdade() {
        return this.idade;
    }
    protected void atualizarCPF(long CPF){
        this.CPF = CPF;
    }
    protected long recuperarCPF(){
        return this.CPF;
    }
    protected void atualizarEndereco(Endereco endereco){
        this.endereco = endereco;
    }
    protected Endereco recuperarEndereco(){
        return this.endereco;
    }
    private int calcularIdade(){
        int lapso;
        Calendar hoje = Calendar.getInstance();
        lapso = hoje.get(Calendar.YEAR) - data_nascimento.get(Calendar.YEAR);
        if ((data_nascimento.get(Calendar.MONTH) > hoje.get(Calendar.MONTH)) ||
                (data_nascimento.get(Calendar.MONTH) == hoje.get(Calendar.MONTH) &&
                        data_nascimento.get(Calendar.DATE) > hoje.get(Calendar.DATE)))
            lapso--;
        return lapso;
    }
}
class Aluno extends Pessoa {
    //Atributos
    private String matricula;
    //Métodos
    public Aluno(String nome, Calendar data_nascimento, long CPF, Endereco endereco){
        super (nome, data_nascimento, CPF, endereco);
    }
}
class Empregado extends Pessoa {
    //Atributos
    protected String matricula;
    private Calendar data_admissao , data_demissao;
    //Métodos
    public Empregado(String nome, Calendar data_nascimento, long CPF, Endereco endereco) {
        super(nome, data_nascimento, CPF, endereco);
        this.matricula = gerarMatricula ();
        data_admissao = Calendar.getInstance();
    }
    public void demitirEmpregado () {
        data_demissao = Calendar.getInstance();
    }
    protected String gerarMatricula () {
        this.matricula = "Matrícula não definida.";
        return this.matricula;
    }
    protected String recuperarMatricula () {
        return this.matricula;
    }
}
public class Principal {
    //Atributos
    private static Aluno aluno;
    private static Endereco endereco;
    //Método main
    public static void main (String args[]) {
        int idade;
        Calendar data = Calendar.getInstance();
        data.set(1980, 10, 23);
        endereco = new Endereco();
        endereco.definirPais("Brasil");
        endereco.definirUF("RJ");
        endereco.definirCidade ("Rio de Janeiro");
        endereco.definirRua("Avenida Rio Branco");
        endereco.definirNumero("156A");
        endereco.definirCEP(20040901);
        endereco.definirComplemento("Bloco 03 - Ap 20.005");
        aluno = new Aluno ("Marco Antônio", data ,901564098 , endereco);
        aluno.atualizarIdade();
        idade = aluno.recuperarIdade();
        System.out.println("Aluno: " + aluno.recuperarNome() + " tem " + idade + " anos.");
        System.out.println("Endereço: " + aluno.recuperarEndereco().getNomeRua() + ", " + aluno.recuperarEndereco().getNumero());
        System.out.println("CPF: " + aluno.recuperarCPF());
        aluno.atualizarNome("Marco Antônio Jr.");
        System.out.println("Aluno: " + aluno.recuperarNome() + " tem " + idade + " anos.");
        System.out.println("Endereço: " + aluno.recuperarEndereco().getNomeRua() + ", " + aluno.recuperarEndereco().getNumero());
        System.out.println("CPF: " + aluno.recuperarCPF());
    }
}
