package com.half.javalearning.threads;

import java.util.Scanner;
import java.util.concurrent.*;
import java.time.Duration;
import java.util.concurrent.StructuredTaskScope;
import java.util.concurrent.StructuredTaskScope.ShutdownOnFailure;

public class VirtualThreads {
    final static ScopedValue<String> USER_ID = ScopedValue.newInstance();

    public static void main(String[] args) throws Exception {
        // 1. Virtual thread demo
        Thread.ofVirtual().start(() -> {
            try {
                System.out.println("Hello from virtual thread");
                Thread.sleep(1000);
            } catch (InterruptedException e) {
                e.printStackTrace();
            } finally {
                System.out.println("Goodbye from virtual thread");
            }
        });

        // 2. Virtual thread executor
        runCounterTask();

        // 3. Structured concurrency
        var response = fetchData(42);
        System.out.println("Structured response: " + response);

        // 4. Scoped value + auth
        ScopedValue.where(USER_ID, "user123").run(() -> {
            auth();
        });

        // 5. Timed question
        question();
    }

    static void runCounterTask() throws InterruptedException {
        int[] counter = {1}; // workaround for lambda capture
        try (var executor = Executors.newVirtualThreadPerTaskExecutor()) {
            Future<String> future = executor.submit(() -> {
                while (counter[0] < 10) {
                    System.out.println("Counter: " + counter[0]);
                    counter[0]++;
                    Thread.sleep(Duration.ofSeconds(1));
                }
                return "Done";
            });
            future.get(); // wait for task to finish
        } catch (ExecutionException e) {
            e.printStackTrace();
        }
    }

    static Response fetchData(int id) throws Exception {
        var fetchUser = (Callable<String>) () -> {
            Thread.sleep(1000);
            return "User" + id;
        };
        var fetchOrder = (Callable<String>) () -> {
            Thread.sleep(1000);
            return "Order" + id;
        };

        try (var scope = new ShutdownOnFailure()) {
            StructuredTaskScope.Subtask<String> userFuture = scope.fork(fetchUser);
            StructuredTaskScope.Subtask<String> orderFuture = scope.fork(fetchOrder);

            scope.join();
            scope.throwIfFailed();

            return new Response(userFuture.get(), orderFuture.get());
        }
    }

    static void auth() {
        System.out.println("Authenticating user: " + USER_ID.get());
        Scanner scanner = new Scanner(System.in);
        System.out.print("Enter password: ");
        String password = scanner.nextLine();
        if (password.equals("password")) {
            System.out.println("User authenticated");
        } else {
            System.out.println("User not authenticated");
        }
        // DO NOT close scanner tied to System.in
    }

    static void question() throws Exception {
        System.out.println("You have 10 seconds to answer");

        int num1 = (int) (Math.random() * 10);
        int num2 = (int) (Math.random() * 10);
        System.out.println("What is " + num1 + " + " + num2 + "?");

        ExecutorService executor = Executors.newVirtualThreadPerTaskExecutor();

        try {
            Future<Integer> userInput = executor.submit(() -> {
                Scanner scanner = new Scanner(System.in);
                return Integer.parseInt(scanner.nextLine());
            });

            Integer answer = userInput.get(10, TimeUnit.SECONDS);

            if (answer == num1 + num2) {
                System.out.println("Correct!");
            } else {
                System.out.println("Wrong!");
            }
        } catch (TimeoutException e) {
            System.out.println("You failed to answer in time");
            System.exit(0);
        } finally {
            executor.shutdownNow();
        }
    }

    record Response(String user, String order) {}
}
