package com.half.javalearning.classes.polymorphism;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.UUID;
import java.util.stream.IntStream;

/**
 * Interface que define propriedades e métodos básicos para funcionários.
 */
interface Empegavel {
    void gerarMatricula();
    String getMatricula();
}

/**
 * Interface que define comportamentos de liderança.
 */
interface Lider extends Empegavel {
    void definirDiretor(Diretor diretor);
    void removerDiretor(Diretor diretor);
    String getDiretorInfo();
    void getDiretoresInfo();
}

/**
 * Classe Empregado que representa um funcionário na empresa.
 * Esta é a superclasse de Diretor.
 */
class Empregado implements Empegavel {
    // Atributos
    protected String nome;
    protected Calendar dataNascimento;
    protected long cpf;
    protected Address endereco;
    protected String matricula;

    /**
     * Construtor para criar um novo objeto Empregado.
     *
     * @param nome            O nome completo do empregado
     * @param dataNascimento  A data de nascimento do empregado
     * @param cpf             O CPF do empregado (documento de identificação)
     * @param endereco        O endereço completo do empregado
     */
    public Empregado(String nome, Calendar dataNascimento, long cpf, Address endereco) {
        this.nome = nome;
        this.dataNascimento = dataNascimento;
        this.cpf = cpf;
        this.endereco = endereco;
        this.matricula = "Matrícula não definida.";
    }

    /**
     * Método para gerar matrícula para um empregado.
     * Este método pode ser sobrescrito por subclasses para
     * implementar comportamentos específicos.
     */
    @Override
    public void gerarMatricula() {
        // Implementação padrão - não faz nada
        // Empregados regulares mantêm "Matrícula não definida."
        int matricula = 0;
        matricula = matricula + this.nome.hashCode();
        matricula = matricula + this.dataNascimento.hashCode();
        matricula = Math.abs(matricula);
        this.matricula = matricula + "";
    }

    /**
     * Retorna a matrícula do empregado.
     *
     * @return A matrícula do empregado
     */
    @Override
    public String getMatricula() {
        return matricula;
    }

    // Outros getters e setters omitidos por brevidade
}

/**
 * Classe Diretor que representa um diretor na empresa.
 * Estende a classe Empregado e adiciona comportamentos específicos de diretores.
 */
class Diretor extends Empregado {
    /**
     * Construtor para criar um novo objeto Diretor.
     *
     * @param nome            O nome completo do diretor
     * @param dataNascimento  A data de nascimento do diretor
     * @param cpf             O CPF do diretor (documento de identificação)
     * @param endereco        O endereço completo do diretor
     */
    public Diretor(String nome, Calendar dataNascimento, long cpf, Address endereco) {
        // Chama o construtor da classe pai (Empregado)
        super(nome, dataNascimento, cpf, endereco);
    }

    /**
     * Gera uma matrícula única para o diretor.
     * Sobrescreve o método da classe pai para usar um formato específico para diretores.
     * O formato usado é "E-" seguido por um UUID aleatório.
     */
    @Override
    public void gerarMatricula() {
        // Verifica se UUID está disponível
        UUID uuid = UUID.randomUUID();
        if (uuid != null) {
            this.matricula = "E-" + uuid.toString();
        } else {
            // Fallback em caso de problema com UUID
            this.matricula = "E-DIR-" + System.currentTimeMillis();
        }
    }

    /**
     * Método para alteração de matrícula sem parâmetros.
     * Quando chamado sem parâmetros, gera uma nova matrícula automática.
     */
    protected void alterarMatricula() {
        gerarMatricula();
    }

    /**
     * Método sobrecarregado para alteração de matrícula com valor específico.
     *
     * @param matricula O novo valor de matrícula a ser atribuído
     */
    protected void alterarMatricula(String matricula) {
        if (matricula != null && !matricula.trim().isEmpty()) {
            this.matricula = matricula;
        } else {
            // Se o valor passado for inválido, gera uma matrícula automática
            gerarMatricula();
        }
    }
}

/**
 * Classe Presidente que representa o presidente da empresa.
 */
class Presidente extends Empregado implements Lider {
    private Diretor diretor;
    private ArrayList<Diretor> diretores;
    private ArrayList<Empregado> empregados;

    /**
     * Construtor para criar um novo Presidente.
     *
     * @param nome            Nome do presidente
     * @param dataNascimento  Data de nascimento
     * @param cpf             CPF do presidente
     * @param endereco        Endereço do presidente
     * @param diretor         Diretor inicial
     */
    public Presidente(String nome, Calendar dataNascimento, long cpf, Address endereco, Diretor diretor) {
        super(nome, dataNascimento, cpf, endereco);
        this.diretor = diretor;
        this.diretores = new ArrayList<>();
        this.empregados = new ArrayList<>(); // Inicializa a lista de empregados

        if (diretor != null) {
            diretores.add(diretor);
        }
    }

    @Override
    public void gerarMatricula() {
        // Valida se o CPF é válido
        if (cpf > 9999999999L) {
            matricula = "P-" + cpf;
        } else {
            matricula = "P-9999999999";
        }
    }

    @Override
    public void definirDiretor(Diretor diretor) {
        if (diretor != null) {
            // Adiciona o diretor à lista de diretores
            diretores.add(diretor);
        }
    }

    @Override
    public void removerDiretor(Diretor diretor) {
        // Remove o diretor da lista de diretores
        diretores.remove(diretor);
    }

    @Override
    public String getDiretorInfo() {
        // Retorna a informação do diretor
        if (diretor != null) {
            return diretor.nome + ", " + diretor.getMatricula();
        }
        return "Nenhum diretor definido";
    }

    @Override
    public void getDiretoresInfo() {
        // Retorna a informação dos diretores
        if (diretores.isEmpty()) {
            System.out.println("Nenhum diretor cadastrado.");
            return;
        }

        StringBuilder sb = new StringBuilder();
        for (Diretor dir : diretores) {
            sb.append(dir.nome).append(", ").append(dir.getMatricula()).append("\n");
        }
        System.out.println(sb.toString());
    }

    /**
     * Adiciona um empregado à lista de empregados.
     *
     * @param empregado Empregado a ser adicionado
     */
    public void adicionarEmpregado(Empregado empregado) {
        if (empregado != null) {
            // Adiciona o empregado à lista de empregados
            empregados.add(empregado);
        }
    }

    /**
     * Remove um empregado da lista de empregados.
     *
     * @param empregado Empregado a ser removido
     */
    public void removerEmpregado(Empregado empregado) {
        // Remove o empregado da lista de empregados
        empregados.remove(empregado);
    }

    /**
     * Retorna informações sobre todos os empregados.
     *
     * @return String com informações dos empregados
     */
    public String getEmpregadosInfo() {
        if (empregados == null || empregados.isEmpty()) {
            return "Nenhum empregado cadastrado.";
        }

        // Retorna a informação dos empregados
        StringBuilder sb = new StringBuilder();
        for (Empregado emp : empregados) {
            sb.append(emp.nome).append(", ").append(emp.getMatricula()).append("\n");
        }
        return sb.toString();
    }
}

class Maior {

    /**
     * Método de teste que demonstra as várias implementações de maiorElem.
     * Cria arrays aleatórios e compara elementos utilizando polimorfismo.
     */
    public void testeMaiorElem() {
        // Criação de arrays aleatórios
        int[] a = gerarArrayAleatorio(100, 100);
        int[] b = gerarArrayAleatorio(100, 100);
        int[] c = gerarArrayAleatorio(100, 100);

        // Exibir arrays para debug
        System.out.println("A: " + Arrays.toString(a));
        System.out.println("B: " + Arrays.toString(b));
        System.out.println("C: " + Arrays.toString(c));

        // Teste do método com três arrays
        int[] result = maiorElem(a, b, c);
        System.out.println("Resultado de maiorElem(a, b, c): " + Arrays.toString(result));

        // Teste do método com dois arrays
        result = maiorElem(a, b);
        System.out.println("Resultado de maiorElem(a, b): " + Arrays.toString(result));

        // Teste do método com um array
        int maiorValorA = maiorElem(a);
        System.out.println("Maior valor em A: " + maiorValorA);

        // Testes com valores individuais
        System.out.println("\nComparando elementos individuais:");
        System.out.println("Comparando maior int: " + a[0] + ", " + b[0] + " e " + c[0]);
        int maior = maiorElem(a[0], b[0], c[0]);
        System.out.println("Maior: " + maior);

        System.out.println("Comparando maior int: " + a[0] + " e " + b[1]);
        maior = maiorElem(a[0], b[1]);
        System.out.println("Maior: " + maior);
    }

    /**
     * Gera um array com valores aleatórios.
     *
     * @param tamanho O tamanho do array a ser gerado
     * @param limiteSuperior O limite superior para os valores aleatórios (exclusivo)
     * @return Um array de inteiros preenchido com valores aleatórios
     */
    private int[] gerarArrayAleatorio(int tamanho, int limiteSuperior) {
        return IntStream.generate(() -> (int) (Math.random() * limiteSuperior))
                .limit(tamanho)
                .toArray();
    }

    /**
     * Encontra o maior elemento de três arrays combinando-os.
     *
     * @param a Primeiro array de inteiros
     * @param b Segundo array de inteiros
     * @param c Terceiro array de inteiros
     * @return Um array contendo os maiores valores combinados
     */
    public int[] maiorElem(int[] a, int[] b, int[] c) {
        // Validação de entrada
        if (a == null || b == null || c == null) {
            throw new IllegalArgumentException("Os arrays não podem ser nulos");
        }

        // Utiliza o método com dois arrays para implementar a funcionalidade
        int[] resultAB = maiorElem(a, b);
        return maiorElem(resultAB, c);
    }

    /**
     * Encontra o maior elemento de dois arrays combinando-os.
     *
     * @param a Primeiro array de inteiros
     * @param b Segundo array de inteiros
     * @return Um array contendo os maiores valores combinados
     */
    public int[] maiorElem(int[] a, int[] b) {
        // Validação de entrada
        if (a == null || b == null) {
            throw new IllegalArgumentException("Os arrays não podem ser nulos");
        }

        // Cria um novo array para armazenar o resultado
        int[] resultado = new int[a.length];

        // Se os tamanhos dos arrays forem diferentes, ajusta para o menor tamanho
        int tamanho = Math.min(a.length, b.length);

        // Escolhe o algoritmo de ordenação adequado baseado no tamanho
        if (tamanho < 100) {
            // Para arrays pequenos, usa um algoritmo simples
            for (int i = 0; i < tamanho; i++) {
                resultado[i] = Math.max(a[i], b[i]);
            }

            // Se 'a' for maior que 'b', completa com os elementos restantes de 'a'
            if (a.length > b.length) {
                System.arraycopy(a, tamanho, resultado, tamanho, a.length - tamanho);
            }
        } else {
            // Para arrays grandes, combina e ordena usando streams
            int[] combinado = IntStream.concat(Arrays.stream(a), Arrays.stream(b))
                    .sorted()
                    .toArray();

            // Pega os maiores valores do array combinado
            System.arraycopy(combinado, combinado.length - tamanho, resultado, 0, tamanho);
        }

        return resultado;
    }

    /**
     * Encontra o maior valor em um array de inteiros.
     *
     * @param a Array de inteiros
     * @return O maior valor no array
     */
    public int maiorElem(int[] a) {
        // Validação de entrada
        if (a == null || a.length == 0) {
            throw new IllegalArgumentException("O array não pode ser nulo ou vazio");
        }

        // Utilizando streams para encontrar o máximo (abordagem funcional)
        return Arrays.stream(a).max().orElse(Integer.MIN_VALUE);
    }

    /**
     * Encontra o maior entre dois valores inteiros.
     *
     * @param a Primeiro valor
     * @param b Segundo valor
     * @return O maior valor entre a e b
     */
    public static int maiorElem(int a, int b) {
        return Math.max(a, b);
    }

    /**
     * Encontra o maior entre três valores inteiros.
     * Utiliza o método que compara dois inteiros.
     *
     * @param a Primeiro valor
     * @param b Segundo valor
     * @param c Terceiro valor
     * @return O maior valor entre a, b e c
     */
    public static int maiorElem(int a, int b, int c) {
        int maiorAB = maiorElem(a, b);
        return maiorElem(maiorAB, c);
    }
}
/**
 * Classe Principal que demonstra o uso de polimorfismo em Java.
 * Implementa métodos que utilizam polimorfismo para encontrar
 * o maior elemento entre dois ou três números.
 */
public class Polymorphism {
    // Atributos para o exemplo de Empregado e Diretor
    private static Empregado empregado, diretor;

    /**
     * Método principal que executa os exemplos de polimorfismo.
     *
     * @param args Argumentos da linha de comando (não utilizados)
     */
    public static void main(String[] args) {
        // Demonstração do polimorfismo com Empregado e Diretor
        demonstrarPolimorfismoEmpregados();

        // Demonstração do polimorfismo com métodos sobrecarregados
        demonstrarPolimorfismoNumeros();
        
        // Instanciação de classe maior
        Maior maior = new Maior();
        
        // Teste de maiorElem
        maior.testeMaiorElem();
        
        // Implementação de Presidente
        Calendar data = Calendar.getInstance();
        data.set(1980, 10, 23);
        
        Presidente presidente = new Presidente("João", data, 211456937, null, new Diretor("Maria", data, 901564098, null));
        System.out.println("Matricula do Presidente: " + presidente.getMatricula());
        System.out.println("Nome do Diretor: " + presidente.getDiretorInfo());
        
        // Adiciona diretor ao presidente
        Diretor diretorDoPresidente = new Diretor("Marco Antônio", data, 901564098, null);
        presidente.definirDiretor(diretorDoPresidente);
        presidente.getDiretoresInfo();
        
        // Adiciona empregado ao presidente
        Empregado empregadoDoPresidente = new Empregado("Clara Silva", data, 211456937, null);
        presidente.adicionarEmpregado(empregadoDoPresidente);
        presidente.getEmpregadosInfo();
    }

    /**
     * Demonstra o polimorfismo de herança com as classes Empregado e Diretor.
     */
    private static void demonstrarPolimorfismoEmpregados() {
        Calendar data = Calendar.getInstance();
        data.set(1980, 10, 23);

        empregado = new Empregado("Clara Silva", data, 211456937, null);
        empregado.gerarMatricula();

        diretor = new Diretor("Marco Antônio", data, 901564098, null);
        diretor.gerarMatricula();

        System.out.println("A matrícula do Diretor é: " + diretor.getMatricula());
        System.out.println("A matrícula do Empregado é: " + empregado.getMatricula());
    }

    /**
     * Demonstra o polimorfismo de sobrecarga com os métodos maiorElem.
     */
    private static void demonstrarPolimorfismoNumeros() {
        // Teste do método maiorElem com dois números
        int a = 5, b = 10, c = 3;

        System.out.println("\nDemonstrando polimorfismo com números:");
        System.out.println("O maior entre " + a + " e " + b + " é: " + maiorElem(a, b));
        System.out.println("O maior entre " + a + ", " + b + " e " + c + " é: " + maiorElem(a, b, c));
    }

    /**
     * Método que retorna o maior elemento entre dois números inteiros.
     *
     * @param a Primeiro número a ser comparado
     * @param b Segundo número a ser comparado
     * @return O maior valor entre a e b
     */
    public static int maiorElem(int a, int b) {
        int maior = a;

        if (b > maior) {
            maior = b;
        }

        return maior;
    }

    /**
     * Método sobrecarregado que retorna o maior elemento entre três números inteiros.
     * Utiliza o método maiorElem(int a, int b) como parte da solução, demonstrando
     * a composição de métodos e o polimorfismo por sobrecarga.
     *
     * @param a Primeiro número a ser comparado
     * @param b Segundo número a ser comparado
     * @param c Terceiro número a ser comparado
     * @return O maior valor entre a, b e c
     */
    public static int maiorElem(int a, int b, int c) {
        // Primeiro encontra o maior entre a e b
        int maior = maiorElem(a, b);

        // Depois compara o maior de a e b com c
        return maiorElem(maior, c);
    }
}

/**
 * Classe para representar um endereço.
 * Utilizada pela classe Empregado e suas subclasses.
 */
class Address {
    private String logradouro;
    private int numero;
    private String complemento;
    private String bairro;
    private String cidade;
    private String estado;
    private String cep;

    /**
     * Construtor para criar um novo endereço.
     *
     * @param logradouro  O nome da rua, avenida, etc.
     * @param numero      O número do endereço
     * @param complemento Informações adicionais sobre o endereço
     * @param bairro      O bairro
     * @param cidade      A cidade
     * @param estado      O estado
     * @param cep         O código postal
     */
    public Address(String logradouro, int numero, String complemento,
                   String bairro, String cidade, String estado, String cep) {
        this.logradouro = logradouro;
        this.numero = numero;
        this.complemento = complemento;
        this.bairro = bairro;
        this.cidade = cidade;
        this.estado = estado;
        this.cep = cep;
    }

    // Getters e setters:
    public String getLogradouro() {
        return logradouro;
    }
    
    public int getNumero() {
        return numero;
    }
    
    public String getComplemento() {
        return complemento;
    }
    
    public String getBairro() {
        return bairro;
    }
    
    public String getCidade() {
        return cidade;
    }
    
    public String getEstado() {
        return estado;
    }
    
    public String getCep() {
        return cep;
    }
}
