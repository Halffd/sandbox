package com.half.javalearning.classes;

import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class Test {
    public static void main(String[] args) throws IOException {
        List letras = new ArrayList();
        letras.add("W");
        letras.add("X");
        letras.add("G");
        letras.add("B");
        letras.add("A");

        Collections.sort(letras);
        System.out.println(letras);
        if (true)
            if (false)
                System.out.println("Teste 1");
            else
                System.out.println("Teste 2");
        FileInputStream file = null;
        try {
            file = new FileInputStream("data.txt");
            // do stuff
            if (true) return; // file still gets closed
        } catch (IOException e) {
            return; // file still gets closed
        } finally {
            System.out.println("Finally runs");
            if (file != null) file.close(); // ALWAYS runs
        }
    }
}
