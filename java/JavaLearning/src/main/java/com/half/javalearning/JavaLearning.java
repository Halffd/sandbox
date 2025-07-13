package com.half.javalearning;

import com.half.javalearning.aluno.AlunoTeste;
import com.half.javalearning.classes.ConcreteExample;

public class JavaLearning {
    public static void main(String[] args) {
        AlunoTeste.criar();
        ConcreteExample concreteExample = new ConcreteExample(10, "Hello");
        System.out.println(concreteExample.getName()+" "+concreteExample.getValue());
        MathUtility.calc(args);
        // HelloServer.start(args);
    }
}