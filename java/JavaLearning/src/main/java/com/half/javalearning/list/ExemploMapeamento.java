package com.half.javalearning.list;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class ExemploMapeamento {
    public static void main(String[] args) {
        List<Integer> numeros = List.of(1, 2, 3, 4, 5);
        List<Integer> quadradoNumeros = map(numeros, x -> x * x);
        System.out.println("Números Originais: " + numeros);
        System.out.println("Números ao Quadrado: " + quadradoNumeros);

        List<Integer> result = scaleList(List.of(1, 2, 3, 4, 5), 10);
        System.out.println(result);

        List<Integer> result2 = mapAdd(List.of(1, 2, 3), List.of(40, 50, 60), List.of(700, 800, 900));
        System.out.println(result2);

        List<Integer> result3 = mapCustom(List.of(1, 2, 3), List.of(4, 5, 6));
        System.out.println(result3);

        // Usage:
        List<Integer> squares = map2(List.of(1, 2, 3, 4), x -> x * x);
// Returns: [1, 4, 9, 16]

        List<Integer> absolute = map2(List.of(-10, 2, -11, 17), Math::abs);
// Returns: [10, 2, 11, 17]
        List<Double> result4 = List.of(3.0, 7.0, 9.0, 3.0, 5.0, 44.0)
                .stream()
                .map(x -> 2 * Math.sin(x))
                .collect(Collectors.toList());
        System.out.println(result4);
    }
    public static <T, R> List<R> map(List<T> lista, Function<T, R> mapa) {
        List<R> resultado = new ArrayList<>();
        for (T item : lista) {
            R itemMapeado = mapa.apply(item);
            resultado.add(itemMapeado);
        }
        return resultado;
    }
    public static List<Integer> scale(List<Integer> items, int factor) {
        return items.stream()
                .map(x -> x * factor)
                .collect(Collectors.toList());
    }
    public static List<Integer> scaleList(List<Integer> items, int factor) {
        return items.stream()
                .map(x -> x * factor)
                .collect(Collectors.toList());
    }
    List<Integer> result = scaleList(List.of(1, 2, 3, 4, 5), 10);
    public static List<Integer> mapAdd2(List<Integer> list1, List<Integer> list2, List<Integer> list3) {
        List<Integer> result = new ArrayList<>();
        for (int i = 0; i < list1.size(); i++) {
            result.add(list1.get(i) + list2.get(i) + list3.get(i));
        }
        return result;
    }
    public static List<Integer> mapAdd(List<Integer> list1, List<Integer> list2, List<Integer> list3) {
        return IntStream.range(0, list1.size())
                .mapToObj(i -> list1.get(i) + list2.get(i) + list3.get(i))
                .collect(Collectors.toList());
    }
    public static List<Integer> mapCustom(List<Integer> list1, List<Integer> list2) {
        return IntStream.range(0, list1.size())
                .mapToObj(i -> list1.get(i) + (2 * list2.get(i)))
                .collect(Collectors.toList());
    }
    public static <T, R> List<R> map2(List<T> items, Function<T, R> proc) {
        if (items.isEmpty()) {
            return new ArrayList<>();
        }
        List<R> result = new ArrayList<>();
        result.add(proc.apply(items.get(0)));
        result.addAll(map2(items.subList(1, items.size()), proc));
        return result;
    }
}
