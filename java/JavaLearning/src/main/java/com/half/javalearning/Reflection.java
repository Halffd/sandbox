package com.half.javalearning;
import java.lang.reflect.*;
import java.util.Arrays;

public class Reflection {
    private String secretField = "I'm private, don't look at me!";
    private static final int MAGIC_NUMBER = 42;

    public static void main(String[] args) {
        try {
            // 1. CLASS INSPECTION
            Class<?> clazz = Reflection.class;
            System.out.println("🔍 Class name: " + clazz.getName());
            System.out.println("🔍 Package: " + clazz.getPackage().getName());
            System.out.println("🔍 Superclass: " + clazz.getSuperclass().getName());

            // 2. CONSTRUCTORS
            System.out.println("\n🏗️ CONSTRUCTORS:");
            Constructor<?>[] constructors = clazz.getDeclaredConstructors();
            Arrays.stream(constructors).forEach(c -> System.out.println("  " + c));

            // Create an instance through reflection
            Constructor<?> constructor = clazz.getDeclaredConstructor();
            Reflection instance = (Reflection) constructor.newInstance();

            // 3. FIELDS - even PRIVATE ones!
            System.out.println("\n🔓 FIELDS (including private!):");
            Field[] fields = clazz.getDeclaredFields();
            for (Field field : fields) {
                field.setAccessible(true); // BREAKING ENCAPSULATION LIKE A BOSS
                System.out.println("  " + field.getName() + " = " + field.get(field.getModifiers() == Modifier.STATIC ? null : instance));

                // Let's modify a private field because we can 😈
                if (field.getName().equals("secretField")) {
                    field.set(instance, "HACKED!");
                    System.out.println("  " + field.getName() + " (after modification) = " + field.get(instance));
                }

                // Modern way to handle finals in Java 9+
                if (field.getName().equals("MAGIC_NUMBER")) {
                    try {
                        // This works differently in Java 9+
                        // Use Java's VarHandle API introduced in Java 9
                        System.out.println("  Attempting to modify final field (may not work in Java 9+)");

                        // For demonstration, we'll try with unsafe (not recommended)
                        // This often requires VM arguments like --add-opens
                        Field unsafeField = Unsafe.class.getDeclaredField("theUnsafe");
                        unsafeField.setAccessible(true);
                        Unsafe unsafe = (Unsafe) unsafeField.get(null);

                        // Get the memory offset of the field
                        long offset = unsafe.staticFieldOffset(field);

                        // Change the value directly in memory
                        unsafe.putInt(Reflection.class, offset, 666);

                        System.out.println("  " + field.getName() + " (after breaking final) = " + field.get(null));
                    } catch (Exception e) {
                        System.out.println("  Failed to modify final field: " + e.getMessage());
                        System.out.println("  This is expected in Java 9+ as Oracle restricted this capability");
                        System.out.println("  Current MAGIC_NUMBER value: " + field.get(null));
                    }
                }
            }

            // 4. METHODS
            System.out.println("\n🧠 METHODS:");
            Method[] methods = clazz.getDeclaredMethods();
            for (Method method : methods) {
                System.out.println("  " + method);
            }

            // Calling methods with different parameter types
            Method secretMethod = clazz.getDeclaredMethod("secretMethod", String.class, int.class);
            secretMethod.setAccessible(true);
            secretMethod.invoke(instance, "Hello from reflection", 123);

            // 5. ANNOTATIONS
            System.out.println("\n🏷️ ANNOTATIONS:");
            if (clazz.isAnnotationPresent(Deprecated.class)) {
                System.out.println("  Class is deprecated!");
            }

            // 6. GENERICS INFO
            System.out.println("\n🧬 GENERIC TYPE INFO:");
            Method genericMethod = clazz.getDeclaredMethod("genericMethod");
            System.out.println("  Return type: " + genericMethod.getGenericReturnType());

            // 7. DYNAMIC PROXY - create classes at runtime!
            System.out.println("\n🦹 DYNAMIC PROXY:");
            SomeInterface proxy = (SomeInterface) Proxy.newProxyInstance(
                    SomeInterface.class.getClassLoader(),
                    new Class<?>[] { SomeInterface.class },
                    (p, m, methodArgs) -> {
                        System.out.println("  Intercepted call to: " + m.getName());
                        return "Proxy response";
                    });
            proxy.doSomething();

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void secretMethod(String message, int value) {
        System.out.println("Secret method says: " + message + " " + value);
    }

    public <T> T genericMethod() {
        return null;
    }
}

interface SomeInterface {
    String doSomething();
}

// For advanced cases only
class Unsafe {
    public native long staticFieldOffset(Field field);
    public native void putInt(Object obj, long offset, int value);
    // Many other dangerous methods...
}