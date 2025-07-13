package com.half.javalearning;

public class CalcSigma {
    private static int soma;
    private static int ind;
    private static int n;
    private static int recursionLevel;
    private static final int MAX_RECURSION_DEPTH = 1000; // Safety limit

    public static void main(String[] args) {
        System.out.println("TESTES DO ALGORITMO calcSigma");
        System.out.println("=============================\n");

        System.out.println("| n | sigma | Descrição |");
        System.out.println("|---|-------|-----------|");

        // Executando 15 testes com valores diferentes de n
        for (int testCase = 1; testCase <= 15; testCase++) {
            // Inicializa as variáveis para cada teste
            soma = 0;
            ind = 1;
            n = testCase;
            recursionLevel = 0;

            // Executa o procedimento recursivo com log detalhado apenas para n=5
            if (testCase == 5) {
                System.out.println("\nLOG DETALHADO PARA n = 5:");
                System.out.println("-------------------------\n");
                calcSigmaWithLog();
                System.out.println("\nFIM DO LOG PARA n = 5\n");
            } else {
                // Execução normal para outros valores
                calcSigma();
            }

            // Registra o resultado final (sigma)
            int sigma = soma;
            System.out.printf("| %2d | %5d | ", n, sigma);

            // Adiciona uma descrição do resultado
            if (sigma == n) {
                System.out.println("sigma = n (soma 1, n vezes)");
            } else {
                System.out.println("Resultado inesperado!");
            }
        }

        System.out.println("\nCONCLUSÃO:");
        System.out.println("O algoritmo calcSigma calcula sigma = n");
        System.out.println("Ou seja, ele simplesmente incrementa 'soma' n vezes (de 1 até n)");
    }

    // Procedimento recursivo calcSigma (versão básica)
    private static void calcSigma() {
        if (ind <= n) {
            soma = soma + 1;
            ind = ind + 1;

            // Verificação de segurança para evitar estouro de pilha
            if (ind > n + 100) {
                System.out.println("AVISO: Possível loop infinito detectado. Forçando retorno.");
                return;
            }

            calcSigma();
        } else {
            return;
        }
    }

    // Versão com log detalhado do procedimento recursivo
    private static void calcSigmaWithLog() {
        recursionLevel++;

        // Verificação de segurança para evitar estouro de pilha
        if (recursionLevel > MAX_RECURSION_DEPTH) {
            System.out.println("ERRO: Profundidade máxima de recursão excedida!");
            return;
        }

        String indent = " ".repeat(recursionLevel * 2);

        System.out.println(indent + "Chamada #" + recursionLevel + ": calcSigma()");
        System.out.println(indent + "Estado atual: soma = " + soma + ", ind = " + ind + ", n = " + n);
        System.out.println(indent + "Verificando condição: ind (" + ind + ") <= n (" + n + ")");

        if (ind <= n) {
            System.out.println(indent + "Condição VERDADEIRA - Executando bloco 'Então'");

            System.out.println(indent + "  Executando: soma = soma + 1");
            soma = soma + 1;
            System.out.println(indent + "  Novo valor: soma = " + soma);

            System.out.println(indent + "  Executando: ind = ind + 1");
            ind = ind + 1;
            System.out.println(indent + "  Novo valor: ind = " + ind);

            System.out.println(indent + "  Executando: chamada recursiva para calcSigma()");
            calcSigmaWithLog();
            System.out.println(indent + "Retornando da chamada recursiva #" + recursionLevel);
        } else {
            System.out.println(indent + "Condição FALSA - Executando bloco 'Senão'");
            System.out.println(indent + "  Executando: retorne");
        }

        recursionLevel--;
    }
}