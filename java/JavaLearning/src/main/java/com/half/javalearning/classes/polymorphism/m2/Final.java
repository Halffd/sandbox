package com.half.javalearning.classes.polymorphism.m2;

//Importações
import java.util.Calendar;
import java.util.regex.Pattern;

/**
 * Main class demonstrating polymorphism with abstract Person class
 * and its concrete subclasses Fisica (Individual) and Juridica (Company).
 */
public class Final {

    // Static array to hold Person references - demonstrates polymorphism
    private static Pessoa[] referencias;

    /**
     * Main method demonstrating polymorphic behavior with abstract classes.
     * Creates instances of Fisica and Juridica, stores them in Pessoa array,
     * and calls abstract methods that are implemented differently in each subclass.
     */
    public static void main(String[] args) {
        try {
            // Initialize array to hold 2 Person references
            referencias = new Pessoa[2];

            // Create calendar instances for birth/creation dates
            Calendar dataNascimento = Calendar.getInstance();
            Calendar dataCriacao = Calendar.getInstance();

            // Set birth date: November 23, 1980 (month is 0-based in Calendar)
            dataNascimento.set(1980, Calendar.NOVEMBER, 23);

            // Create individual person (CPF holder)
            referencias[0] = new Fisica(
                    "Marco Antônio",
                    dataNascimento,
                    null,  // endereco
                    null,  // telefone
                    "Brasil",
                    "Rio de Janeiro"
            );

            // Set company creation date: March 1, 1913
            dataCriacao.set(1913, Calendar.MARCH, 1);

            // Create legal entity (CNPJ holder)
            referencias[1] = new Juridica(
                    "Escola Novo Mundo Ltda",
                    dataCriacao,
                    null,  // endereco
                    null,  // telefone
                    "Brasil",
                    "Rio de Janeiro"
            );

            // Update IDs - polymorphic method calls
            // Each subclass has its own validation logic
            boolean cpfValido = referencias[0].atualizarID("365.586.875-45");
            boolean cnpjValido = referencias[1].atualizarID("43.186.666/0026-32");

            // Display results
            System.out.println("=== DEMONSTRAÇÃO DE POLIMORFISMO ===");
            System.out.println("CPF atualizado: " + cpfValido);
            System.out.println("CNPJ atualizado: " + cnpjValido);

            // Show polymorphic behavior - same method call, different implementations
            for (int i = 0; i < referencias.length; i++) {
                System.out.println("Pessoa " + i + ": " + referencias[i].recuperarID());
                System.out.println("Tipo: " + referencias[i].getClass().getSimpleName());
            }

        } catch (Exception e) {
            System.err.println("Erro na execução: " + e.getMessage());
            e.printStackTrace();
        }
    }
}

/**
 * Abstract base class representing a Person.
 * This class cannot be instantiated directly - demonstrates abstraction.
 * Defines common interface for all person types while allowing specialized behavior.
 */
abstract class Pessoa {

    // Protected fields accessible by subclasses
    protected String nome;
    protected Calendar data;
    protected String endereco;
    protected String telefone;
    protected String pais;
    protected String estado;

    // Private field - encapsulation
    private String identificador;

    /**
     * Constructor for Person base class.
     *
     * @param nome Name of the person/entity
     * @param data Birth date for individuals, creation date for companies
     * @param endereco Address (can be null)
     * @param telefone Phone number (can be null)
     * @param pais Country
     * @param estado State/Province
     */
    protected Pessoa(String nome, Calendar data, String endereco,
                     String telefone, String pais, String estado) {

        if (nome == null || nome.trim().isEmpty()) {
            throw new IllegalArgumentException("Nome não pode ser vazio");
        }
        if (data == null) {
            throw new IllegalArgumentException("Data não pode ser nula");
        }
        if (pais == null || pais.trim().isEmpty()) {
            throw new IllegalArgumentException("País não pode ser vazio");
        }

        this.nome = nome.trim();
        this.data = (Calendar) data.clone(); // Defensive copy
        this.endereco = endereco;
        this.telefone = telefone;
        this.pais = pais.trim();
        this.estado = estado;
        this.identificador = null; // Will be set by subclasses
    }

    /**
     * Abstract method for updating identification.
     * Each subclass implements its own validation logic:
     * - Fisica validates CPF format
     * - Juridica validates CNPJ format
     *
     * @param identificador The ID string to validate and set
     * @return true if ID is valid and was set, false otherwise
     */
    protected abstract boolean atualizarID(String identificador);

    /**
     * Returns the current identification number.
     * This method is final - cannot be overridden by subclasses.
     *
     * @return The identification string, or null if not set
     */
    protected final String recuperarID() {
        return this.identificador;
    }

    /**
     * Protected method to set the identifier after validation.
     * Only accessible by subclasses.
     *
     * @param identificador The validated identifier to set
     */
    protected final void setIdentificador(String identificador) {
        this.identificador = identificador;
    }

    // Additional getters for accessing protected fields
    public String getNome() { return nome; }
    public Calendar getData() { return (Calendar) data.clone(); }
    public String getEndereco() { return endereco; }
    public String getTelefone() { return telefone; }
    public String getPais() { return pais; }
    public String getEstado() { return estado; }

    @Override
    public String toString() {
        return String.format("%s{nome='%s', id='%s', pais='%s'}",
                getClass().getSimpleName(), nome, identificador, pais);
    }
}

/**
 * Concrete class representing an individual person (holds CPF).
 * Specializes the abstract Pessoa class with CPF-specific validation.
 */
class Fisica extends Pessoa {

    // Regex pattern for basic CPF format validation
    private static final Pattern CPF_PATTERN = Pattern.compile("\\d{3}\\.\\d{3}\\.\\d{3}-\\d{2}");

    public Fisica(String nome, Calendar dataNascimento, String endereco,
                  String telefone, String pais, String estado) {
        super(nome, dataNascimento, endereco, telefone, pais, estado);
    }

    /**
     * Implements CPF validation and update logic.
     * Validates format and checksum digits according to CPF algorithm.
     *
     * @param cpf The CPF string to validate
     * @return true if CPF is valid and was set, false otherwise
     */
    @Override
    protected boolean atualizarID(String cpf) {
        if (cpf == null || cpf.trim().isEmpty()) {
            System.err.println("CPF não pode ser vazio");
            return false;
        }

        String cpfLimpo = cpf.trim();

        // Basic format validation
        if (!CPF_PATTERN.matcher(cpfLimpo).matches()) {
            System.err.println("Formato de CPF inválido: " + cpfLimpo);
            return false;
        }

        // Additional validation could include checksum verification
        if (validarCPF(cpfLimpo)) {
            setIdentificador(cpfLimpo);
            return true;
        } else {
            System.err.println("CPF inválido: " + cpfLimpo);
            return false;
        }
    }

    /**
     * Validates CPF checksum digits.
     * Implements the Brazilian CPF validation algorithm.
     */
    private boolean validarCPF(String cpf) {
        // Remove formatting
        String numeros = cpf.replaceAll("[^0-9]", "");

        // Check for known invalid patterns
        if (numeros.length() != 11 || numeros.matches("(\\d)\\1{10}")) {
            return false;
        }

        try {
            // Calculate first verification digit
            int soma = 0;
            for (int i = 0; i < 9; i++) {
                soma += Character.getNumericValue(numeros.charAt(i)) * (10 - i);
            }
            int primeiroDigito = 11 - (soma % 11);
            if (primeiroDigito >= 10) primeiroDigito = 0;

            // Calculate second verification digit
            soma = 0;
            for (int i = 0; i < 10; i++) {
                soma += Character.getNumericValue(numeros.charAt(i)) * (11 - i);
            }
            int segundoDigito = 11 - (soma % 11);
            if (segundoDigito >= 10) segundoDigito = 0;

            // Verify both digits
            return Character.getNumericValue(numeros.charAt(9)) == primeiroDigito &&
                    Character.getNumericValue(numeros.charAt(10)) == segundoDigito;

        } catch (Exception e) {
            return false;
        }
    }
}

/**
 * Concrete class representing a legal entity/company (holds CNPJ).
 * Specializes the abstract Pessoa class with CNPJ-specific validation.
 */
class Juridica extends Pessoa {

    // Regex pattern for basic CNPJ format validation
    private static final Pattern CNPJ_PATTERN = Pattern.compile("\\d{2}\\.\\d{3}\\.\\d{3}/\\d{4}-\\d{2}");

    public Juridica(String razaoSocial, Calendar dataCriacao, String endereco,
                    String telefone, String pais, String estado) {
        super(razaoSocial, dataCriacao, endereco, telefone, pais, estado);
    }

    /**
     * Implements CNPJ validation and update logic.
     * Validates format and checksum digits according to CNPJ algorithm.
     *
     * @param cnpj The CNPJ string to validate
     * @return true if CNPJ is valid and was set, false otherwise
     */
    @Override
    protected boolean atualizarID(String cnpj) {
        if (cnpj == null || cnpj.trim().isEmpty()) {
            System.err.println("CNPJ não pode ser vazio");
            return false;
        }

        String cnpjLimpo = cnpj.trim();

        // Basic format validation
        if (!CNPJ_PATTERN.matcher(cnpjLimpo).matches()) {
            System.err.println("Formato de CNPJ inválido: " + cnpjLimpo);
            return false;
        }

        // Additional validation could include checksum verification
        if (validarCNPJ(cnpjLimpo)) {
            setIdentificador(cnpjLimpo);
            return true;
        } else {
            System.err.println("CNPJ inválido: " + cnpjLimpo);
            return false;
        }
    }

    /**
     * Validates CNPJ checksum digits.
     * Implements the Brazilian CNPJ validation algorithm.
     */
    private boolean validarCNPJ(String cnpj) {
        // Remove formatting
        String numeros = cnpj.replaceAll("[^0-9]", "");

        if (numeros.length() != 14 || numeros.matches("(\\d)\\1{13}")) {
            return false;
        }

        try {
            // First verification digit
            int[] peso1 = {5, 4, 3, 2, 9, 8, 7, 6, 5, 4, 3, 2};
            int soma = 0;
            for (int i = 0; i < 12; i++) {
                soma += Character.getNumericValue(numeros.charAt(i)) * peso1[i];
            }
            int primeiroDigito = soma % 11 < 2 ? 0 : 11 - (soma % 11);

            // Second verification digit
            int[] peso2 = {6, 5, 4, 3, 2, 9, 8, 7, 6, 5, 4, 3, 2};
            soma = 0;
            for (int i = 0; i < 13; i++) {
                soma += Character.getNumericValue(numeros.charAt(i)) * peso2[i];
            }
            int segundoDigito = soma % 11 < 2 ? 0 : 11 - (soma % 11);

            // Verify both digits
            return Character.getNumericValue(numeros.charAt(12)) == primeiroDigito &&
                    Character.getNumericValue(numeros.charAt(13)) == segundoDigito;

        } catch (Exception e) {
            return false;
        }
    }
}