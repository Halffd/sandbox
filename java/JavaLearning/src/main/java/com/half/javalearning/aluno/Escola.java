package com.half.javalearning.aluno;
/**
 * Representa uma instituição de ensino com seus departamentos e alunos.
 * Implementa relações de composição com Departamento e agregação com Aluno.
 */
class Escola {
    // Atributos privados para encapsulamento
    private String nome;
    private String cnpj;
    private Endereco endereco; // Associação
    private Departamento[] departamentos; // Composição
    private AlunoEscola[] discentes;
    private int numDiscentes;
    private int numDepartamentos;

    // Constantes para definir tamanhos máximos
    private static final int MAX_DEPARTAMENTOS = 10;
    private static final int MAX_DISCENTES = 1000;

    /**
     * Construtor da classe Escola.
     * Inicializa uma escola com nome e CNPJ, criando arrays vazios para departamentos e alunos.
     *
     * @param nome O nome da instituição
     * @param cnpj O CNPJ da instituição
     */
    public Escola(String nome, String cnpj) {
        this.nome = nome;
        this.cnpj = cnpj;
        this.departamentos = new Departamento[MAX_DEPARTAMENTOS];
        this.discentes = new AlunoEscola[MAX_DISCENTES];
        this.numDiscentes = 0;
        this.numDepartamentos = 0;
    }
    public String[] getDiscentes() {
        String[] discentesNames = new String[numDiscentes];
        for (int i = 0; i < numDiscentes; i++) {
            discentesNames[i] = discentes[i].getNome();
        }
        return discentesNames;
    }

    /**
     * Define o endereço da escola.
     *
     * @param endereco O objeto Endereco da escola
     */
    public void setEndereco(Endereco endereco) {
        this.endereco = endereco;
    }

    /**
     * Adiciona um departamento à escola (relação de composição).
     *
     * @param departamento O departamento a ser adicionado
     * @return true se o departamento foi adicionado com sucesso, false caso contrário
     */
    public boolean adicionarDepartamento(Departamento departamento) {
        if (numDepartamentos < MAX_DEPARTAMENTOS) {
            departamentos[numDepartamentos] = departamento;
            numDepartamentos++;
            return true;
        }
        return false;
    }

    /**
     * Matricula um aluno na escola (relação de agregação).
     *
     * @param aluno O aluno a ser matriculado
     * @return true se o aluno foi matriculado com sucesso, false caso contrário
     */
    public boolean matricularAluno(AlunoEscola aluno) {
        if (numDiscentes < MAX_DISCENTES) {
            discentes[numDiscentes] = aluno;
            numDiscentes++;
            return true;
        }
        return false;
    }

    // Getters
    public String getNome() {
        return nome;
    }

    public String getCnpj() {
        return cnpj;
    }

    public Endereco getEndereco() {
        return endereco;
    }

    public int getNumDiscentes() {
        return numDiscentes;
    }

    public int getNumDepartamentos() {
        return numDepartamentos;
    }
    public static void main(String[] args) {
        // Criar uma escola aleatória
        Escola escola = criarEscolaAleatoria();

        // Exibir informações da escola
        System.out.println("=== INFORMAÇÕES DA ESCOLA ===");
        System.out.println("Nome: " + escola.getNome());
        System.out.println("CNPJ: " + escola.getCnpj());
        System.out.println("Endereço: " + escola.getEndereco().getNomeRua() + ", " + escola.getEndereco().getNumero());
        System.out.println("Número de Departamentos: " + escola.getNumDepartamentos());
        System.out.println("Número de Alunos: " + escola.getNumDiscentes());
        for(int studentIndex = 0; studentIndex < escola.getNumDiscentes(); studentIndex++) {
            AlunoEscola aluno = escola.discentes[studentIndex];
            System.out.println("Aluno " + (studentIndex + 1) + ": " + aluno.getNome());
        }
        System.out.println("\n=== SIMULAÇÃO CONCLUÍDA COM SUCESSO ===");
    }

    /**
     * Cria uma escola com dados aleatórios
     * @return Uma instância de Escola populada com dados aleatórios
     */
    private static Escola criarEscolaAleatoria() {
        // Arrays com possíveis valores para geração aleatória
        String[] nomesEscolas = {"Colégio Futuro Brilhante", "Instituto Educacional Horizonte",
                "Escola Técnica Inovação", "Centro de Ensino Progresso",
                "Faculdade Vale do Saber"};

        String[] cnpjs = {"12.345.678/0001-90", "98.765.432/0001-10",
                "45.678.912/0001-34", "78.912.345/0001-56",
                "23.456.789/0001-87"};

        String[] nomesRuas = {"Avenida da Educação", "Rua do Conhecimento",
                "Praça do Saber", "Alameda dos Estudantes",
                "Estrada da Ciência"};

        String[] nomesDepartamentos = {"Ciências Exatas", "Humanidades",
                "Tecnologia", "Ciências Biológicas",
                "Artes e Comunicação", "Educação Física",
                "Línguas Estrangeiras"};

        String[] nomesAlunos = {"Ana Silva", "Pedro Santos", "Juliana Oliveira",
                "Carlos Ferreira", "Mariana Costa", "João Pereira",
                "Beatriz Lima", "Lucas Martins", "Gabriela Sousa",
                "Rafael Almeida"};

        // Gerar uma escola com nome e CNPJ aleatórios
        String nomeEscola = nomesEscolas[(int)(Math.random() * nomesEscolas.length)];
        String cnpj = cnpjs[(int)(Math.random() * cnpjs.length)];
        Escola escola = new Escola(nomeEscola, cnpj);

        // Criar e definir um endereço aleatório
        String nomeRua = nomesRuas[(int)(Math.random() * nomesRuas.length)];
        int numero = 100 + (int)(Math.random() * 900); // Número entre 100 e 999
        Endereco endereco = new Endereco(nomeRua, numero);
        escola.setEndereco(endereco);

        // Criar e adicionar departamentos aleatórios (entre 3 e 5)
        int numDepartamentos = 3 + (int)(Math.random() * 3);
        for (int i = 0; i < numDepartamentos; i++) {
            String nomeDepartamento = nomesDepartamentos[i % nomesDepartamentos.length];
            Departamento departamento = new Departamento(nomeDepartamento, "DEP-" + (i+100));
            escola.adicionarDepartamento(departamento);
        }

        // Matricular alunos aleatórios (entre 15 e 30)
        int numAlunos = 15 + (int)(Math.random() * 16);
        for (int i = 0; i < numAlunos; i++) {
            String nomeAluno = nomesAlunos[i % nomesAlunos.length] + " " + (i+1);
            String matricula = "A" + (2000 + i);
            int idade = 18 + (int)(Math.random() * 12); // Idade entre 18 e 29
            AlunoEscola aluno = new AlunoEscola(nomeAluno, matricula, idade);
            escola.matricularAluno(aluno);
        }

        return escola;
    }
}

/**
 * Classe que representa um endereço
 */
class Endereco {
    private String nomeRua;
    private int numero;

    public Endereco(String nomeRua, int numero) {
        this.nomeRua = nomeRua;
        this.numero = numero;
    }

    public String getNomeRua() {
        return nomeRua;
    }

    public int getNumero() {
        return numero;
    }
    public static void main(String args[]){
        Endereco ender = new Endereco ("rua X", 7);
        System.out.println(ender.getNomeRua()+", "+ender.getNumero());
    }
}

/**
 * Classe que representa um departamento (composição com Escola)
 */
class Departamento {
    private String nome;
    private String codigo;

    public Departamento(String nome, String codigo) {
        this.nome = nome;
        this.codigo = codigo;
    }

    public String getNome() {
        return nome;
    }

    public String getCodigo() {
        return codigo;
    }
}

/**
 * Classe que representa um aluno (agregação com Escola)
 */
class AlunoEscola {
    private String nome;
    private String matricula;
    private int idade;

    public AlunoEscola(String nome, String matricula, int idade) {
        this.nome = nome;
        this.matricula = matricula;
        this.idade = idade;
    }

    public String getNome() {
        return nome;
    }

    public String getMatricula() {
        return matricula;
    }

    public int getIdade() {
        return idade;
    }
}
