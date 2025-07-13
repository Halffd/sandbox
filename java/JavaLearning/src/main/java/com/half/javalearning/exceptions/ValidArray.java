package com.half.javalearning.exceptions;

class ErroValidacao extends Throwable {
    ErroValidacao ( String msg_erro ) {
        super ( msg_erro );
    }
    ErroValidacao ( String msg_erro , Throwable causa ) {
        super ( msg_erro , causa );
    }
    public void atribuirCausa ( Throwable causa ) {
        initCause ( causa );
    }
    @Override
    public String toString ( ) {
        return "ErroValidacao: " + this.getMessage();
    }
}
public class ValidArray {
    public static void main(String args[]) throws ErroValidacao {
        OperacaoArray calc = new OperacaoArray();
        char[] op1 = null;
        char[] op2 = null;

        try {
            System.out.println("Tentando criar vetores...");
            // Try to create huge arrays that might cause OutOfMemoryError
            op1 = new char[Short.MAX_VALUE];
            op2 = new char[Short.MAX_VALUE];
            System.out.println("Vetores criados com sucesso!");

        } catch (OutOfMemoryError e) {
            Runtime runtime = Runtime.getRuntime();
            System.out.println("Memoria insuficiente!");
            System.out.println("A memória total da MVJ eh " + runtime.totalMemory() +
                    " e o máximo eh " + runtime.maxMemory());
            System.out.println("Reconfigure a MVJ usando o parametro -Xmx<size>. " +
                    "Você precisa de " + (2L * Short.MAX_VALUE * 2) +
                    " bytes para os vetores.");
            System.exit(-1);
        } finally {
            System.out.println("Finally executado");
        }

        if (op1 != null && op2 != null) {
            char[] result = calc.concatenarArray(op1, op2);
            System.out.println("Concatenação realizada. Tamanho resultado: " + result.length);
        }
    }
}

class OperacaoArray {

    public char[] concatenarArray ( char[] op1 , char[] op2 ) throws ErroValidacao {
        int tamnh_res;

        tamnh_res = op1.length + op2.length;

        return copiarArray ( op1 , op2 , tamnh_res , op2.length );

    }
    private char[] copiarArray ( char[] op1 , char[] op2 , int tamnh_res , int n ){
        char[] resultado = new char [ tamnh_res ];
        System.arraycopy ( op1 , 0 , resultado , 0 , op1.length );
        System.arraycopy ( op2 , 0 , resultado , op1.length , n );
        return resultado;
    }
}