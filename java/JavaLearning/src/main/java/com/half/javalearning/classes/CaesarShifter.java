package com.half.javalearning.classes;

public class CaesarShifter {
    public static String caesarCipher(String text, int shift) {
        return text.chars()
                .mapToObj(c -> (char) c)
                .map(c -> Character.isLetter(c) ?
                        (char) ((c - (Character.isUpperCase(c) ? 'A' : 'a') + shift + 26) % 26 +
                                (Character.isUpperCase(c) ? 'A' : 'a')) : c)
                .collect(StringBuilder::new, StringBuilder::append, StringBuilder::append)
                .toString();
    }
    public static String decodeCaesarCipher(String text, int shift) {
        return caesarCipher(text, -shift);
    }
    public static void main(String[] args) {
        System.out.println(caesarCipher("Hello", 1));
        System.out.println(decodeCaesarCipher("Ifmmp", 1));
    }
}
