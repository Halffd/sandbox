package com.half.javalearning.classes;

public class Wrappers {
    public static void  main(String[] args) {
        // Wrapper classes = Allow primitive values (int, char, double, boolean)
        //                                   to be used as objects. "Wrap them in an object"
        //                                   Generally, don't wrap primitives unless you need an object.
        //                                   Allows use of Collections Framework and static utility methods.

        //int a = 123;
/*        Integer a = new Integer(123);
        Double b = new Double(3.14);
        Character c = new Character('@');
        Boolean d = new Boolean(false);*/

        //Autoboxing
        Integer a1 = 123;
        Double a2 = 3.14;
        Character a3 = '@';
        Boolean a4 = true;
        String e = "pizza";

        //unboxing
        int x = a1;
        double y = a2;
        char k = a3;
        boolean z = a4;

        String a = Integer.toString(x);
        String b = Double.toString(y);
        String c = Character.toString(k);
        String d = Boolean.toString(z);

        String i = a + b + c + d + e;
        System.out.println(i);

        int ba = Integer.parseInt("123");
        double bb = Double.parseDouble("3.14");
        char cc = "Pizza".charAt(0);
        boolean dd = Boolean.parseBoolean("true");

        char letter = '&';

        System.out.println(Character.isLetter(letter));
        System.out.println(Character.isUpperCase(letter));
        
        System.out.println(Character.toUpperCase(letter));
        System.out.println(Character.toLowerCase(letter));
    }
}