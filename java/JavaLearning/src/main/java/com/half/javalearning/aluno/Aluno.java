package com.half.javalearning.aluno;

import java.util.Random; // Importing the Random class.

public class Aluno {
    private String nome;
    private int idade;
    private double codigo_identificador;
    private Random aleatorio;
    public Aluno() {
        aleatorio = new Random(); // Initializing the Random object.

        // Generating a random name with two parts.
        this.nome = generateRandomName(3, 15) + " " + generateRandomName(3, 15);

        // Generating a random age between 1 and 100.
        this.idade = aleatorio.nextInt(100) + 1;

        // Generating a random identifier (double).
        this.codigo_identificador = aleatorio.nextDouble();
    }
    public String getNome() {
        return this.nome;
    }
    // Method to generate a random name following the consonant-vowel rule.
    private String generateRandomName(int minLength, int maxLength) {
        int length = aleatorio.nextInt(maxLength - minLength + 1) + minLength; // Random length.
        StringBuilder sb = new StringBuilder();

        boolean isConsonant = true; // Start with a consonant.
        for (int i = 0; i < length; i++) {
            char randomChar;
            if (isConsonant) {
                randomChar = getRandomConsonant(); // Get a random consonant.
            } else {
                randomChar = getRandomVowel(); // Get a random vowel.
            }
            sb.append(Character.toLowerCase(randomChar));
            isConsonant = !isConsonant; // Alternate between consonant and vowel.
        }

        // Capitalize the first letter of the name.
        sb.setCharAt(0, Character.toUpperCase(sb.charAt(0)));

        return sb.toString();
    }

    // Method to get a random consonant.
    private char getRandomConsonant() {
        String consonants = "BCDFGHJKLMNPQRSTVWXYZ";
        return consonants.charAt(aleatorio.nextInt(consonants.length()));
    }

    // Method to get a random vowel.
    private char getRandomVowel() {
        String vowels = "AEIOU";
        return vowels.charAt(aleatorio.nextInt(vowels.length()));
    }

    // Method to display student details.
    public void exibirDetalhes() {
        System.out.println("Nome: " + nome);
        System.out.println("Idade: " + idade);
        System.out.println("Código Identificador: " + codigo_identificador);
    }

    /**
     * The main method serves as the entry point for the application.
     * It creates a new instance of AlunoRandom and displays its details
     * using the exibirDetalhes method.
     *
     * @param args command line arguments (not used in this method)
     */
    public static void main(String[] args) {
        // Create a new Aluno object and display its details.
        Aluno aluno = new Aluno();
        aluno.exibirDetalhes();


        char c = 'X';

        // Check if it's a letter.
        System.out.println("Is letter? " + Character.isLetter(c));

        // Convert to lowercase.
        System.out.println("Lowercase: " + Character.toLowerCase(c));

        // Check if it's a digit.
        System.out.println("Is digit? " + Character.isDigit(c));

        // Get the Unicode code point.
        System.out.println("Code point: " + Character.codePointAt(new char[]{c}, 0));
        String[] command = {"ls", "-al"};
        try {
            Process process = Runtime.getRuntime().exec(command);
            Runtime runtime = Runtime.getRuntime();
            System.out.println("Available processors: " + runtime.availableProcessors());
            System.out.println("Free memory: " + runtime.freeMemory());
            System.out.println("Total memory: " + runtime.totalMemory());
            System.out.println("Max memory: " + runtime.maxMemory());
            System.gc();
            System.out.println("Free memory after garbage collection: " + runtime.freeMemory());
        } catch (Exception e) {
            System.err.println("Error executing command: " + e.getMessage());
        }
        System.gc();
    }
}
