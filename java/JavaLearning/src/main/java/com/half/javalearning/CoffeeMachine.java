package com.half.javalearning;

import java.util.function.Consumer;
import java.util.concurrent.CompletableFuture;
import java.util.Random;

class Coffee {
    String blend;
    boolean decaf;
    int price;

    Coffee(int price, String blend, boolean decaf) {
        this.price = price;
        this.blend = blend;
        this.decaf = decaf; // The devil's boolean
    }

    String brew() {
        if (decaf) {
            return "☕ (but sad)";
        }
        return blend + " coffee brewing... ☕";
    }
}

class CoffeeMachine {
    private Consumer<Coffee> coffeeListener;

    public void listen(Consumer<Coffee> listener) {
        this.coffeeListener = listener;
        System.out.println("Machine is listening... but your lambda is dead inside");
    }

    public void brew(Coffee coffee) {
        System.out.println("Brewing: " + coffee.brew());

        // Notify the listener (who will do absolutely nothing)
        if (coffeeListener != null) {
            coffeeListener.accept(coffee);
        }
    }
    public static void main(String[] args) {
        CoffeeMachine coffeeMachine = new CoffeeMachine();

        // The lambda that does absolutely nothing, as requested
        coffeeMachine.listen((coffee) -> {
            // *crickets chirping*
            // This is where dreams go to die
            // *crickets chirping*
            System.out.println("Lambda is dead inside");
            Random random = new Random();
            int chance = random.nextInt(100);
            if (chance > 50) {
                // We're going to sleep, but we're not actually going to sleep
                try {
                    System.out.println("Sleeping...");
                    Thread.sleep(1000);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }
        });

        // Actually make some coffee for demonstration
        Coffee actualCoffee = new Coffee(450, "Dark Roast", false);
        coffeeMachine.brew(actualCoffee);
    }
}