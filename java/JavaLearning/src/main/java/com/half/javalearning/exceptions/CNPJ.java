package com.half.javalearning.exceptions;

import java.util.Calendar;

// Missing Pessoa class (parent class)
abstract class Pessoa {
    protected String nome;
    protected Calendar dataCriacao;
    protected String identificador;
    protected Endereco endereco;
    protected String nacionalidade;
    protected String sede;

    public Pessoa(String nome, Calendar dataCriacao, String identificador,
                  Endereco endereco, String nacionalidade, String sede) {
        this.nome = nome;
        this.dataCriacao = dataCriacao;
        this.identificador = identificador;
        this.endereco = endereco;
        this.nacionalidade = nacionalidade;
        this.sede = sede;
    }

    // Abstract method that subclasses must implement
    public abstract boolean atualizarID(String id) throws ErroValidacaoCNPJ;
}

// Missing Endereco class
class Endereco {
    private String rua;
    private String cidade;
    private String estado;
    private String cep;

    public Endereco() {
        // Default constructor
    }

    public Endereco(String rua, String cidade, String estado, String cep) {
        this.rua = rua;
        this.cidade = cidade;
        this.estado = estado;
        this.cep = cep;
    }

    // Getters and setters
    public String getRua() { return rua; }
    public void setRua(String rua) { this.rua = rua; }

    public String getCidade() { return cidade; }
    public void setCidade(String cidade) { this.cidade = cidade; }

    public String getEstado() { return estado; }
    public void setEstado(String estado) { this.estado = estado; }

    public String getCep() { return cep; }
    public void setCep(String cep) { this.cep = cep; }
}
// Custom Exception Class
class ErroValidacaoCNPJ extends Exception {
    private String msgErro;

    public ErroValidacaoCNPJ(String msgErro) {
        super(msgErro);
        this.msgErro = msgErro;
    }

    @Override
    public String toString() {
        return "ErroValidacaoCNPJ: " + msgErro;
    }
}

// Main Juridica Class
class Juridica extends Pessoa {

    public Juridica(String razaoSocial, Calendar dataCriacao, String CNPJ,
                    Endereco endereco, String nacionalidade, String sede) {
        super(razaoSocial, dataCriacao, CNPJ, endereco, nacionalidade, sede);
    }

    @Override
    public boolean atualizarID(String CNPJ) throws ErroValidacaoCNPJ {
        if (validaCNPJ(CNPJ)) {
            this.identificador = CNPJ;
            return true;
        }
        return false; // This is technically unreachable due to exception throwing
    }

    private boolean validaCNPJ(String CNPJ) throws ErroValidacaoCNPJ {
        // Input validation
        if (CNPJ == null || CNPJ.length() != 14 || isSequenceOfSameDigits(CNPJ)) {
            throw new ErroValidacaoCNPJ("Entrada inválida!");
        }

        try {
            char DV13, DV14;
            int soma, num, peso, resto;

            // First verification digit calculation
            soma = 0;
            peso = 2;
            for (int i = 11; i >= 0; i--) {
                num = Character.getNumericValue(CNPJ.charAt(i));
                soma += (num * peso);
                peso++;
                if (peso == 10) {
                    peso = 2;
                }
            }

            resto = soma % 11;
            if (resto == 0 || resto == 1) {
                DV13 = '0';
            } else {
                DV13 = (char)((11 - resto) + 48);
            }

            // Second verification digit calculation
            soma = 0;
            peso = 2;
            for (int i = 12; i >= 0; i--) {
                num = Character.getNumericValue(CNPJ.charAt(i));
                soma += (num * peso);
                peso++;
                if (peso == 10) {
                    peso = 2;
                }
            }

            resto = soma % 11;
            if (resto == 0 || resto == 1) {
                DV14 = '0';
            } else {
                DV14 = (char)((11 - resto) + 48);
            }

            // Verify calculated digits match input digits
            if (DV13 == CNPJ.charAt(12) && DV14 == CNPJ.charAt(13)) {
                return true;
            } else {
                throw new ErroValidacaoCNPJ("DV inválido.");
            }

        } catch (NumberFormatException | StringIndexOutOfBoundsException e) {
            throw new ErroValidacaoCNPJ("Formato de CNPJ inválido: " + e.getMessage());
        }
    }

    private boolean isSequenceOfSameDigits(String cnpj) {
        // Check if all digits are the same (00000000000000, 11111111111111, etc.)
        return cnpj.matches("(\\d)\\1{13}");
    }

    public String retornaTipo() {
        return "Juridica";
    }
}

// Example usage class
public class CNPJ {
    public static void main(String[] args) {
        try {
            // You'd need to implement these classes or mock them
            Endereco endereco = new Endereco();
            Calendar data = Calendar.getInstance();

            Juridica empresa = new Juridica("Empresa Teste", data,
                    "11222333000181", endereco,
                    "Brasil", "São Paulo");

            // Test valid CNPJ
            boolean resultado = empresa.atualizarID("11222333000181");
            System.out.println("CNPJ válido: " + resultado);

            // Test invalid CNPJ - this will throw exception
            empresa.atualizarID("00000000000000");

        } catch (ErroValidacaoCNPJ e) {
            System.out.println("Erro capturado: " + e.toString());
        }
    }
}
