package com.half.javalearning.list;
import java.util.*;
import java.util.stream.Collectors;

class Aluno {
    //Atributos
    private String matricula,nome,naturalidade;
    //Métodos
    public Aluno(String nome,String naturalidade){
        this.nome=nome;
        this.naturalidade=naturalidade;
    }
    public String recuperarNaturalidade(){
        return naturalidade;
    }
     
    @Override
    public String toString(){
        return String.format("%s(%s)",nome,naturalidade);
    }
}
class Endereco{
    //Atributos
    private String rua,bairro,cidade,estado,cep;
    //Métodos
    public Endereco(String rua,String bairro,String cidade,String estado,String cep){
        this.rua=rua;
        this.bairro=bairro;
        this.cidade=cidade;
        this.estado=estado;
        this.cep=cep;
    }
    @Override
    public String toString(){
        return String.format("%s, %s, %s, %s, %s",rua,bairro,cidade,estado,cep);
    }
}
class Departamento{
    //Atributos
    private String nome;
    private List alunos;
    //Métodos
    public Departamento(String nome){
        this.nome=nome;
        this.alunos=new ArrayList<Aluno>();
    }
    public void adicionarAluno(Aluno aluno){
        alunos.add(aluno);
    }
    public void removerAluno(Aluno aluno){
        alunos.remove(aluno);
    }
    public List recuperarAlunos(){
        return alunos;
    }
    //toString
    @Override
    public String toString(){
        return String.format("%s(%d alunos)",nome,alunos.size());
    }
}
class Escola{
    //Atributos
    private String nome,CNPJ;
    private Endereco endereco;
    private List departamentos;
    private List<Aluno> discentes = new ArrayList<>();
    //Métodos
    public Escola(String nome,String CNPJ){
        this.nome=nome;
        this.CNPJ=CNPJ;
        this.departamentos=new ArrayList<Departamento>();
        this.discentes=new ArrayList<Aluno>();
    }
    public void mostrarDiscentes(){
        for (Aluno a: discentes){
            System.out.println(a);
        }
    }
    public void criarDepartamento(String nomeDepartamento){
        departamentos.add(new Departamento(nomeDepartamento));
    }
    public void fecharDepartamento(Departamento departamento){
        departamentos.remove(departamento);
    }
    public void matricularAluno(Aluno novoAluno){
        discentes.add(novoAluno);
    }
    public void trancarMatriculaAluno(Aluno aluno){
        discentes.remove(aluno);
    }
    public void agruparAlunos(){
        Map<String,List<Aluno>> agrupamento=new HashMap<>();
        for (Aluno a: discentes){
            if(!agrupamento.containsKey(a.recuperarNaturalidade())) {
                agrupamento.put(a.recuperarNaturalidade(),new ArrayList<>());
            }
            agrupamento.get(a.recuperarNaturalidade()).add(a);
        }
        System.out.println ("Resultado do agrupamento por naturalidade: "+agrupamento);
        Map<String,List<Aluno>> agrupamento2=
                discentes.stream().collect(Collectors.groupingBy(Aluno::recuperarNaturalidade));
        System.out.println("Resultado do agrupamento por naturalidade: ");
        agrupamento2.forEach((String chave,List<Aluno> lista)->System.out.println(chave+" = "+lista));
        Map<String, Set<Aluno>>agrupamento3 =
                discentes.stream().collect(Collectors.groupingBy(Aluno::recuperarNaturalidade,Collectors.toSet()));
        System.out.println("Resultado do agrupamento por naturalidade: ");
        agrupamento3.forEach((String chave,Set<Aluno>conjunto)-> System.out.println(chave+" = "+conjunto));
        Map<String,Set<Aluno>> agrupamento4=discentes.stream().collect(Collectors.groupingBy(Aluno::recuperarNaturalidade,TreeMap::new,Collectors.toSet()));
        System.out.println("Resultado do agrupamento por naturalidade: ");
        agrupamento4.forEach((String chave,Set<Aluno>conjunto)-> System.out.println(chave+" = "+conjunto));
    }
}
public class Principal {
    // Atributos
    private static Aluno aluno1,aluno2,aluno3,aluno4,aluno5,aluno6,aluno7,aluno8,aluno9;
    private static Escola escola;
    // Método main
    public static void main(String args[]) {
        escola = new Escola("Escola Pedro Álvares Cabral", "42.336.174/0006-13");
        criarAlunos();
        matricularAlunos();
        escola.agruparAlunos();
        // printa os alunos matriculados
        escola.mostrarDiscentes();
    }
    //Métodos
    private static void criarAlunos( ){
        aluno1 = new Aluno("Marco Antônio","Rio de Janeiro");
        aluno2 = new Aluno("Clara Silva","Rio de Janeiro");
        aluno3 = new Aluno("Marcos Cintra","Sorocaba");
        aluno4 = new Aluno("Ana Beatriz","Barra do Pirai");
        aluno5 = new Aluno("Marcio Gomes","São Paulo");
        aluno6 = new Aluno("João Carlos","Sorocaba");
        aluno7 = new Aluno("César Augusto","São Paulo");
        aluno8 = new Aluno("Alejandra Gomez","Madri");
        aluno9 = new Aluno("Castelo Branco","São Paulo");
    }
    private static void matricularAlunos( ){
        escola.matricularAluno(aluno1);
        escola.matricularAluno(aluno2);
        escola.matricularAluno(aluno3);
        escola.matricularAluno(aluno4);
        escola.matricularAluno(aluno5);
        escola.matricularAluno(aluno6);
        escola.matricularAluno(aluno7);
        escola.matricularAluno(aluno8);
        escola.matricularAluno(aluno9);
    }
}