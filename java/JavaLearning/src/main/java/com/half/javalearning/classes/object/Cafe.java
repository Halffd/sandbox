package com.half.javalearning.classes.object;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ThreadLocalRandom;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;

public class Cafe {
    private final String cafeName;
    private final Map<String, Maid> maids = new ConcurrentHashMap<>();
    private final Map<String, Customer> customers = new ConcurrentHashMap<>();
    private final Queue<Order> orderQueue = new LinkedList<>();
    private final List<String> menuItems = new ArrayList<>();
    private int totalRevenue = 0;
    private CafeState currentState = CafeState.CLOSED;

    // Weeb level metrics (because of course we track this)
    private int headpatsGiven = 0;
    private int nyaCount = 0;
    private int embarrassmentLevel = 0;

    public Cafe(String cafeName) {
        this.cafeName = cafeName;
        initializeMenu();
        System.out.println("🎀 Welcome to " + cafeName + "! Kyaa~! 🎀");
    }

    /**
     * Maid class - The heart and soul of degeneracy
     */
    public static class Maid {
        private final String name;
        private final MaidType type;
        private final MaidPersonality personality;
        private int energy = 100;
        private int cuteness = ThreadLocalRandom.current().nextInt(85, 101); // All maids are cute!
        private int embarrassment = 0;
        private final List<String> catchphrases = new ArrayList<>();
        private boolean isWorking = false;

        public Maid(String name, MaidType type, MaidPersonality personality) {
            this.name = name;
            this.type = type;
            this.personality = personality;
            generateCatchphrases();
        }

        private void generateCatchphrases() {
            switch (personality) {
                case TSUNDERE:
                    catchphrases.addAll(Arrays.asList(
                            "It's not like I made this specially for you or anything! B-baka!",
                            "Don't get the wrong idea! I'm just doing my job!",
                            "Y-you better appreciate this!"
                    ));
                    break;
                case DANDERE:
                    catchphrases.addAll(Arrays.asList(
                            "*whispers* I hope you like it...",
                            "*blushes* Please enjoy your meal...",
                            "*quietly* Thank you for coming..."
                    ));
                    break;
                case GENKI:
                    catchphrases.addAll(Arrays.asList(
                            "Kyaa~! This is so exciting!",
                            "Master! Master! Try this, nya~!",
                            "Let's make today super fun, desu!"
                    ));
                    break;
                case KUUDERE:
                    catchphrases.addAll(Arrays.asList(
                            "Here is your order. It's adequate.",
                            "I suppose you deserve decent service.",
                            "Don't expect special treatment."
                    ));
                    break;
            }
        }

        public String serve() {
            if (!isWorking) return name + " is taking a break, nya~";

            energy -= 10;
            String catchphrase = catchphrases.get(ThreadLocalRandom.current().nextInt(catchphrases.size()));

            if (energy < 20) {
                return name + " is getting tired... *yawn* " + catchphrase;
            }

            return "🌸 " + name + " says: \"" + catchphrase + "\" 🌸";
        }

        public void receiveHeadpat() {
            embarrassment += 15;
            cuteness += 5;
            if (cuteness > 100) cuteness = 100;

            switch (personality) {
                case TSUNDERE:
                    System.out.println(name + ": \"W-what are you doing?! Don't touch my head, baka!\" *blushes furiously*");
                    break;
                case DANDERE:
                    System.out.println(name + ": *blushes and looks down* \"T-thank you...\"");
                    break;
                case GENKI:
                    System.out.println(name + ": \"Ehehe~ That feels nice, Master!\"");
                    break;
                case KUUDERE:
                    System.out.println(name + ": \"...I suppose this is... acceptable.\" *tries to hide blush*");
                    break;
            }
        }

        public void rest() {
            energy = Math.min(100, energy + 30);
            embarrassment = Math.max(0, embarrassment - 10);
            isWorking = false;
        }

        // Getters and setters with maximum weeb energy
        public String getName() { return name; }
        public MaidType getType() { return type; }
        public int getCuteness() { return cuteness; }
        public int getEnergy() { return energy; }
        public boolean isWorking() { return isWorking; }
        public void setWorking(boolean working) { this.isWorking = working; }
    }

    /**
     * Customer class - The degenerates who keep us in business
     */
    public static class Customer {
        private final String name;
        private final CustomerType type;
        private int money;
        private int satisfaction = 50;
        private boolean isSimping = false;

        public Customer(String name, CustomerType type, int money) {
            this.name = name;
            this.type = type;
            this.money = money;
        }

        public String react(String maidService) {
            satisfaction += ThreadLocalRandom.current().nextInt(10, 25);

            switch (type) {
                case OTAKU:
                    return name + ": \"KAWAII DESU NE~! This is the best day ever!\"";
                case NORMAL:
                    return name + ": \"The service here is... interesting.\"";
                case WEEB:
                    isSimping = true;
                    return name + ": \"*nosebleed* This is what heaven must be like!\"";
                case BUSINESSMAN:
                    return name + ": \"Efficient service. I approve.\"";
                default:
                    return name + ": \"...\"";
            }
        }

        public boolean canAfford(int price) {
            return money >= price;
        }

        public void pay(int amount) {
            money -= amount;
        }

        // Getters
        public String getName() { return name; }
        public CustomerType getType() { return type; }
        public int getSatisfaction() { return satisfaction; }
        public boolean isSimping() { return isSimping; }
    }

    /**
     * Order class - The glue that holds this chaos together
     */
    public static class Order {
        private final String customerName;
        private final String item;
        private final int price;
        private final String specialRequest;
        private OrderStatus status = OrderStatus.PENDING;

        public Order(String customerName, String item, int price, String specialRequest) {
            this.customerName = customerName;
            this.item = item;
            this.price = price;
            this.specialRequest = specialRequest;
        }

        // Getters and setters
        public String getCustomerName() { return customerName; }
        public String getItem() { return item; }
        public int getPrice() { return price; }
        public String getSpecialRequest() { return specialRequest; }
        public OrderStatus getStatus() { return status; }
        public void setStatus(OrderStatus status) { this.status = status; }
    }

    // Enums for maximum type safety and organization
    public enum MaidType {
        CATGIRL, FOXGIRL, BUNNY, DRAGON, ANGEL, DEMON, ROBOT, WITCH
    }

    public enum MaidPersonality {
        TSUNDERE, DANDERE, GENKI, KUUDERE, YANDERE // Yandere maids are... dangerous
    }

    public enum CustomerType {
        OTAKU, NORMAL, WEEB, BUSINESSMAN, TOURIST, REGULAR
    }

    public enum CafeState {
        OPEN, CLOSED, RUSH_HOUR, MAID_BREAK, CHAOS_MODE
    }

    public enum OrderStatus {
        PENDING, PREPARING, READY, SERVED, BLESSED_BY_MAID
    }

    // Core cafe operations

    public void hireMaid(String name, MaidType type, MaidPersonality personality) {
        if (maids.containsKey(name)) {
            System.out.println("A maid named " + name + " already works here, nya~");
            return;
        }

        Maid newMaid = new Maid(name, type, personality);
        maids.put(name, newMaid);

        System.out.println("🎉 Welcome to the team, " + name + "-chan! 🎉");
        System.out.println("Type: " + type + " | Personality: " + personality);

        // Special hiring messages
        if (type == MaidType.CATGIRL) {
            nyaCount += 50; // Catgirls increase nya count just by existing
        }
    }

    public void addCustomer(String name, CustomerType type, int money) {
        Customer customer = new Customer(name, type, money);
        customers.put(name, customer);

        System.out.println("Welcome to " + cafeName + ", " + name + "-sama!");

        if (type == CustomerType.WEEB) {
            System.out.println("*Internal weeb radar starts beeping*");
        }
    }

    public void takeOrder(String customerName, String item, String specialRequest) {
        if (!customers.containsKey(customerName)) {
            System.out.println("Customer not found! Are they hiding in the bathroom?");
            return;
        }

        if (!menuItems.contains(item)) {
            System.out.println("We don't serve " + item + " here, desu~");
            return;
        }

        int price = calculatePrice(item);
        Customer customer = customers.get(customerName);

        if (!customer.canAfford(price)) {
            System.out.println(customerName + " can't afford " + item + "! Time to wash dishes, nya~");
            return;
        }

        Order order = new Order(customerName, item, price, specialRequest);
        orderQueue.add(order);

        System.out.println("Order taken: " + item + " for " + customerName + "-sama");
        if (!specialRequest.isEmpty()) {
            System.out.println("Special request: " + specialRequest);
        }
    }

    public void serveOrders() {
        if (orderQueue.isEmpty()) {
            System.out.println("No orders to serve! Time for maid dance break! ♪(´▽｀)♪");
            return;
        }

        List<Maid> availableMaids = maids.values().stream()
                .filter(Maid::isWorking)
                .filter(maid -> maid.getEnergy() > 15)
                .toList();

        if (availableMaids.isEmpty()) {
            System.out.println("All maids are exhausted! Emergency maid break initiated!");
            return;
        }

        while (!orderQueue.isEmpty() && !availableMaids.isEmpty()) {
            Order order = orderQueue.poll();
            Maid servingMaid = availableMaids.get(ThreadLocalRandom.current().nextInt(availableMaids.size()));

            String serviceMessage = servingMaid.serve();
            Customer customer = customers.get(order.getCustomerName());

            customer.pay(order.getPrice());
            totalRevenue += order.getPrice();

            String customerReaction = customer.react(serviceMessage);

            System.out.println("🍰 " + servingMaid.getName() + " serves " + order.getItem() + " to " + customer.getName());
            System.out.println(serviceMessage);
            System.out.println(customerReaction);

            order.setStatus(OrderStatus.SERVED);

            // Handle special requests with maximum chaos
            if (!order.getSpecialRequest().isEmpty()) {
                handleSpecialRequest(order.getSpecialRequest(), servingMaid, customer);
            }
        }
    }

    private void handleSpecialRequest(String request, Maid maid, Customer customer) {
        String lowerRequest = request.toLowerCase();

        if (lowerRequest.contains("cute") || lowerRequest.contains("kawaii")) {
            System.out.println(maid.getName() + " does an extra cute pose! (｡◕‿◕｡)");
            customer.satisfaction += 20;
        }

        if (lowerRequest.contains("nya") || lowerRequest.contains("meow")) {
            System.out.println(maid.getName() + ": \"Nya nya~! (=^･ω･^=)\"");
            nyaCount += 5;
        }

        if (lowerRequest.contains("headpat") || lowerRequest.contains("pat")) {
            System.out.println(customer.getName() + " attempts to headpat " + maid.getName() + "!");
            maid.receiveHeadpat();
            headpatsGiven++;
            embarrassmentLevel += 10;
        }

        if (lowerRequest.contains("dance")) {
            System.out.println(maid.getName() + " performs a magical maid dance! ✨(ﾉ◕ヮ◕)ﾉ*:･ﾟ✧");
            customer.satisfaction += 30;
        }

        if (lowerRequest.contains("marry") || lowerRequest.contains("love")) {
            System.out.println(maid.getName() + ": \"E-eh?! W-what are you saying?!\" *steam comes out of ears*");
            maid.embarrassment += 50;
            embarrassmentLevel += 25;
            currentState = CafeState.CHAOS_MODE;
        }
    }

    public void openCafe() {
        currentState = CafeState.OPEN;
        maids.values().forEach(maid -> maid.setWorking(true));

        System.out.println("🎀✨ " + cafeName + " is now OPEN! ✨🎀");
        System.out.println("Current time: " + LocalTime.now().format(DateTimeFormatter.ofPattern("HH:mm")));
        System.out.println("Maids on duty: " + maids.size());
        System.out.println("Let the kawaii chaos begin! (ﾉ◕ヮ◕)ﾉ*:･ﾟ✧");
    }

    public void closeCafe() {
        currentState = CafeState.CLOSED;
        maids.values().forEach(maid -> {
            maid.setWorking(false);
            maid.rest();
        });

        System.out.println("🌙 " + cafeName + " is now CLOSED! 🌙");
        printDailyStats();
    }

    public void giveHeadpat(String maidName) {
        if (!maids.containsKey(maidName)) {
            System.out.println("That maid doesn't exist! Are you hallucinating waifus again?");
            return;
        }

        Maid maid = maids.get(maidName);
        maid.receiveHeadpat();
        headpatsGiven++;

        System.out.println("*pat pat* ♡");

        // Random chance for extra chaos
        if (ThreadLocalRandom.current().nextInt(100) < 10) {
            System.out.println("💥 CRITICAL HEADPAT! " + maidName + " is now at maximum cuteness! 💥");
            maid.cuteness = 100;
        }
    }

    public void triggerMaidEvent() {
        if (maids.isEmpty()) {
            System.out.println("No maids to trigger events with! Hire some first, you absolute walnut!");
            return;
        }

        List<String> maidNames = new ArrayList<>(maids.keySet());
        String randomMaidName = maidNames.get(ThreadLocalRandom.current().nextInt(maidNames.size()));
        Maid randomMaid = maids.get(randomMaidName);

        MaidEvent event = MaidEvent.values()[ThreadLocalRandom.current().nextInt(MaidEvent.values().length)];

        System.out.println("🎭 RANDOM MAID EVENT ACTIVATED! 🎭");

        switch (event) {
            case CLUMSY_MOMENT:
                System.out.println(randomMaidName + " trips and spills tea everywhere!");
                System.out.println("\"Kyaa~! I'm so sorry, Master!\" *bows repeatedly*");
                randomMaid.embarrassment += 30;
                embarrassmentLevel += 15;
                break;

            case SUPER_KAWAII_MODE:
                System.out.println(randomMaidName + " enters MAXIMUM KAWAII MODE!");
                System.out.println("\"Nya nya~! ♡(˃͈ દ ˂͈ ༶ )♡\"");
                randomMaid.cuteness = 100;
                nyaCount += 20;
                break;

            case TSUNDERE_EXPLOSION:
                System.out.println(randomMaidName + " has a tsundere moment!");
                System.out.println("\"It's not like I care about you or anything! B-BAKA!\" *throws apron*");
                randomMaid.embarrassment += 40;
                break;

            case MAID_SOLIDARITY:
                System.out.println("All maids unite for a group hug!");
                System.out.println("\"We're all in this together, desu~!\"");
                maids.values().forEach(maid -> {
                    maid.energy = Math.min(100, maid.energy + 20);
                    maid.embarrassment = Math.max(0, maid.embarrassment - 10);
                });
                break;

            case FORBIDDEN_TECHNIQUE:
                System.out.println(randomMaidName + " uses the forbidden technique: ULTIMATE CUTE EYES!");
                System.out.println("\"Please... enjoy your stay, Master~\" *sparkly eyes*");
                customers.values().forEach(customer -> customer.satisfaction += 50);
                break;

            case MAID_REBELLION:
                System.out.println("The maids are rebelling! They demand more headpats!");
                System.out.println("\"No more service until we get proper appreciation!\"");
                currentState = CafeState.CHAOS_MODE;
                break;
        }
    }

    public void activateChaosMode() {
        currentState = CafeState.CHAOS_MODE;
        System.out.println("🔥💥 CHAOS MODE ACTIVATED! 💥🔥");
        System.out.println("All hell breaks loose in the maid cafe!");

        // Random chaos events
        for (int i = 0; i < 3; i++) {
            triggerMaidEvent();
        }

        System.out.println("The cafe is now in complete pandemonium!");
        System.out.println("Emergency cute protocol initiated!");

        // All maids get maximum cuteness to restore order
        maids.values().forEach(maid -> maid.cuteness = 100);
    }

    public void printDailyStats() {
        System.out.println("\n📊 DAILY MAID CAFE STATISTICS 📊");
        System.out.println("═══════════════════════════════════");
        System.out.println("💰 Total Revenue: ¥" + totalRevenue);
        System.out.println("👋 Headpats Given: " + headpatsGiven);
        System.out.println("🐱 Nya Count: " + nyaCount);
        System.out.println("😳 Embarrassment Level: " + embarrassmentLevel + "%");
        System.out.println("👥 Customers Served: " + customers.size());
        System.out.println("👸 Maids Employed: " + maids.size());

        System.out.println("\n🏆 MAID PERFORMANCE REPORT 🏆");
        maids.values().forEach(maid -> {
            System.out.println("• " + maid.getName() + " (" + maid.getType() + "): " +
                    "Cuteness: " + maid.getCuteness() + "% | " +
                    "Energy: " + maid.getEnergy() + "%");
        });

        System.out.println("\n💖 CUSTOMER SATISFACTION 💖");
        customers.values().forEach(customer -> {
            String status = customer.isSimping() ? " (SIMPING DETECTED)" : "";
            System.out.println("• " + customer.getName() + ": " +
                    customer.getSatisfaction() + "%" + status);
        });

        // Calculate cafe grade
        double avgSatisfaction = customers.values().stream()
                .mapToInt(Customer::getSatisfaction)
                .average().orElse(0);

        String grade = avgSatisfaction >= 90 ? "S++ ULTIMATE KAWAII" :
                avgSatisfaction >= 80 ? "A+ VERY KAWAII" :
                        avgSatisfaction >= 70 ? "B+ KAWAII" :
                                avgSatisfaction >= 60 ? "C+ SOMEWHAT KAWAII" : "F NEEDS MORE KAWAII";

        System.out.println("\n🌟 CAFE GRADE: " + grade + " 🌟");
    }

    private void initializeMenu() {
        menuItems.addAll(Arrays.asList(
                "Moe Moe Kyun Cake", "Tsundere Tiramisu", "Kawaii Katsu Curry",
                "Neko Neko Noodles", "Magical Girl Matcha", "Otaku Omurice",
                "Waifu Waffle", "Senpai Sandwich", "Baka Burger", "Desu Desu Drink",
                "Headpat Hot Chocolate", "Nya Nya Nachos", "Onii-chan Onigiri"
        ));
    }

    private int calculatePrice(String item) {
        // Kawaii tax is real
        Map<String, Integer> prices = Map.ofEntries(
                Map.entry("Moe Moe Kyun Cake", 1200),
                Map.entry("Tsundere Tiramisu", 1000),
                Map.entry("Kawaii Katsu Curry", 1500),
                Map.entry("Neko Neko Noodles", 900),
                Map.entry("Magical Girl Matcha", 800),
                Map.entry("Otaku Omurice", 1300),
                Map.entry("Waifu Waffle", 1100),
                Map.entry("Senpai Sandwich", 700),
                Map.entry("Baka Burger", 1400),
                Map.entry("Desu Desu Drink", 600),
                Map.entry("Headpat Hot Chocolate", 850),
                Map.entry("Nya Nya Nachos", 950),
                Map.entry("Onii-chan Onigiri", 750)
        );

        return prices.getOrDefault(item, 1000);
    }

    // Additional chaos events
    public enum MaidEvent {
        CLUMSY_MOMENT, SUPER_KAWAII_MODE, TSUNDERE_EXPLOSION,
        MAID_SOLIDARITY, FORBIDDEN_TECHNIQUE, MAID_REBELLION
    }

    // Getters for the brave souls who want to interact with this system
    public String getCafeName() { return cafeName; }
    public int getTotalRevenue() { return totalRevenue; }
    public int getHeadpatsGiven() { return headpatsGiven; }
    public int getNyaCount() { return nyaCount; }
    public CafeState getCurrentState() { return currentState; }
    public Map<String, Maid> getMaids() { return new HashMap<>(maids); }
    public Map<String, Customer> getCustomers() { return new HashMap<>(customers); }

    /**
     * The main method - Where dreams come to die and waifus come to life
     */
    public static void main(String[] args) {
        System.out.println("🎌 INITIALIZING MAXIMUM WEEB EXPERIENCE 🎌");

        Cafe cafe = new Cafe("Kawaii Desu Cafe ~Nya");

        // Hire some maids (the essential workforce)
        cafe.hireMaid("Sakura-chan", MaidType.CATGIRL, MaidPersonality.TSUNDERE);
        cafe.hireMaid("Yuki-chan", MaidType.FOXGIRL, MaidPersonality.DANDERE);
        cafe.hireMaid("Rei-chan", MaidType.BUNNY, MaidPersonality.KUUDERE);
        cafe.hireMaid("Miku-chan", MaidType.ROBOT, MaidPersonality.GENKI);

        // Add some customers (the paying degenerates)
        cafe.addCustomer("Otaku-kun", CustomerType.WEEB, 5000);
        cafe.addCustomer("Businessman-san", CustomerType.BUSINESSMAN, 10000);
        cafe.addCustomer("Normie-kun", CustomerType.NORMAL, 3000);

        // Open the cafe
        cafe.openCafe();

        // Take some orders
        cafe.takeOrder("Otaku-kun", "Moe Moe Kyun Cake", "please be extra kawaii");
        cafe.takeOrder("Businessman-san", "Otaku Omurice", "make it efficient");
        cafe.takeOrder("Normie-kun", "Senpai Sandwich", "nya please");

        // Serve the orders
        cafe.serveOrders();

        // Give some headpats (essential for maid happiness)
        cafe.giveHeadpat("Sakura-chan");
        cafe.giveHeadpat("Yuki-chan");

        // Trigger random chaos
        cafe.triggerMaidEvent();
        cafe.triggerMaidEvent();

        // Activate ultimate chaos (for science)
        cafe.activateChaosMode();

        // Close the cafe
        cafe.closeCafe();

        System.out.println("\n🎭 END OF WEEB SIMULATION 🎭");
        System.out.println("Thank you for experiencing maximum degeneracy!");
        System.out.println("Remember: 2D > 3D, and headpats are life! (´∀｀)♡");
    }
}
