package com.half.javalearning;

import java.util.Map;
import java.util.function.Function;

public class Print<T> {
    private static final String RESET = "\033[0m";
    private static String delimiter = " ";
    private static boolean newLine = true;

    private static final Map<String, String> COLORS = Map.of(
            "red", "\033[31m",
            "green", "\033[32m",
            "yellow", "\033[33m",
            "blue", "\033[34m",
            "purple", "\033[35m",
            "cyan", "\033[36m",
            "white", "\033[37m",
            "reset", "\033[0m"
    );

    public enum Level {
        DEBUG("\033[36m", "[D]"),
        INFO("\033[32m", "   "),
        WARNING("\033[33m", "[W]"),
        ERROR("\033[31m", "[E]");

        private final String color;
        private final String prefix;

        Level(String color, String prefix) {
            this.color = color;
            this.prefix = prefix;
        }

        public String getColor() { return color; }
        public String getPrefix() { return prefix; }
    }

    // Generic formatter interface
    public interface Formatter<T> {
        String format(T item);
    }

    // Default formatter for any object
    private static final Formatter<Object> DEFAULT_FORMATTER = Object::toString;

    // Generic delimiter setter
    public static <D> void setDelimiter(D delimiter) {
        Print.delimiter = delimiter.toString();
    }

    // Generic newline setter
    public static <B> void setNewLine(B newLine) {
        if (newLine instanceof Boolean) {
            Print.newLine = (Boolean) newLine;
        } else {
            Print.newLine = Boolean.parseBoolean(newLine.toString());
        }
    }

    // Generic print method with custom formatter
    @SafeVarargs
    private static <T> void printWithLevel(Level level, boolean addNewline,
                                           Formatter<T> formatter, T... args) {
        StringBuilder out = new StringBuilder();
        out.append(level.getColor()).append(level.getPrefix()).append(" ");

        for (int i = 0; i < args.length; i++) {
            out.append(formatter.format(args[i]));
            if (i < args.length - 1) {
                out.append(delimiter);
            }
        }

        if (addNewline) {
            out.append("\n");
        }
        out.append(RESET);

        System.out.print(out.toString());
    }

    // Overloaded version using default formatter
    @SafeVarargs
    private static <T> void printWithLevel(Level level, boolean addNewline, T... args) {
        printWithLevel(level, addNewline, (Formatter<T>) DEFAULT_FORMATTER, args);
    }

    // Generic print methods
    @SafeVarargs
    public static <T> void print(T... args) {
        printWithLevel(Level.INFO, newLine, args);
    }

    @SafeVarargs
    public static <T> void println(T... args) {
        printWithLevel(Level.INFO, true, args);
    }

    @SafeVarargs
    public static <T> void debug(T... args) {
        printWithLevel(Level.DEBUG, newLine, args);
    }

    @SafeVarargs
    public static <T> void info(T... args) {
        printWithLevel(Level.INFO, newLine, args);
    }

    @SafeVarargs
    public static <T> void warning(T... args) {
        printWithLevel(Level.WARNING, newLine, args);
    }

    @SafeVarargs
    public static <T> void error(T... args) {
        printWithLevel(Level.ERROR, newLine, args);
    }

    @SafeVarargs
    public static <T> void debugln(T... args) {
        printWithLevel(Level.DEBUG, true, args);
    }

    @SafeVarargs
    public static <T> void infoln(T... args) {
        printWithLevel(Level.INFO, true, args);
    }

    @SafeVarargs
    public static <T> void warningln(T... args) {
        printWithLevel(Level.WARNING, true, args);
    }

    @SafeVarargs
    public static <T> void errorln(T... args) {
        printWithLevel(Level.ERROR, true, args);
    }

    // Generic print with custom formatter
    @SafeVarargs
    public static <T> void printFormatted(Formatter<T> formatter, T... args) {
        printWithLevel(Level.INFO, newLine, formatter, args);
    }

    @SafeVarargs
    public static <T> void printlnFormatted(Formatter<T> formatter, T... args) {
        printWithLevel(Level.INFO, true, formatter, args);
    }

    // Generic printf - this one actually benefits from generics
    public static <T> void printf(String format, T... args) {
        System.out.printf(format, (Object[]) args);
    }

    // Generic collection printer
    public static <T, C extends Iterable<T>> void printCollection(C collection) {
        print("[");
        boolean first = true;
        for (T item : collection) {
            if (!first) {
                print(", ");
            }
            print(item);
            first = false;
        }
        println("]");
    }

    // Generic map printer
    public static <K, V, M extends Map<K, V>> void printMap(M map) {
        println("{");
        map.forEach((key, value) ->
                println("  " + key + " -> " + value)
        );
        println("}");
    }

    // Generic conditional printer
    public static <T> void printIf(boolean condition, T... args) {
        if (condition) {
            print(args);
        }
    }

    // Generic transformation printer
    public static <T, R> void printTransformed(Function<T, R> transformer, T... args) {
        @SuppressWarnings("unchecked")
        R[] transformed = (R[]) new Object[args.length];
        for (int i = 0; i < args.length; i++) {
            transformed[i] = transformer.apply(args[i]);
        }
        print(transformed);
    }

    public static void main(String[] args) {
        // Basic usage - now with generics!
        Print.<String>print("Hello", "World", "!");
        Print.<Integer>println(1, 2, 3, 4, 5);

        // Mixed types
        print("Number:", 42, "Boolean:", true, "Float:", 3.14f);

        // Custom formatter
        Formatter<Integer> hexFormatter = i -> "0x" + Integer.toHexString(i);
        printlnFormatted(hexFormatter, 255, 16, 32);

        // Level-specific printing
        debug("Debug:", 123);
        info("Info:", "message");
        warning("Warning:", true);
        error("Error:", 404);

        // Collection printing
        printCollection(java.util.List.of("a", "b", "c"));

        // Map printing
        printMap(Map.of("key1", "value1", "key2", "value2"));

        // Conditional printing
        printIf(true, "This will print");
        printIf(false, "This won't print");

        // Transformation printing
        printTransformed(String::toUpperCase, "hello", "world");

        // Generic printf
        printf("Formatted: %s %d %f\n", "test", 42, 3.14);
    }
}