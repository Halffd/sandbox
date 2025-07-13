package com.half.javalearning.strings;

import java.util.*;
import java.util.regex.*;
import java.util.concurrent.ThreadLocalRandom;

// Base vehicle class - because everything's a car in traffic hell
abstract class Vehicle {
    protected String licensePlate;
    protected String driverName;
    protected double roadRageLevel;
    protected int speedLimit;
    protected String[] honkSounds;
    protected Pattern platePattern;

    public Vehicle(String licensePlate, String driverName, double roadRageLevel, String... honkSounds) {
        this.licensePlate = validateLicensePlate(licensePlate);
        this.driverName = sanitizeDriverName(driverName);
        this.roadRageLevel = Math.max(0, Math.min(10, roadRageLevel));
        this.speedLimit = ThreadLocalRandom.current().nextInt(25, 80);
        this.honkSounds = honkSounds.length > 0 ? honkSounds : new String[]{"BEEP", "HOOOONK"};
        this.platePattern = Pattern.compile("^[A-Z0-9]{2,8}$");
    }

    // REGEX MADNESS - License plate validation
    private String validateLicensePlate(String plate) {
        if (plate == null) return "NULL404";

        // Remove all non-alphanumeric and convert to uppercase
        String cleaned = plate.replaceAll("[^A-Za-z0-9]", "").toUpperCase();

        // Meme plate patterns
        String[] memePatterns = {
                ".*69.*", ".*420.*", ".*YOLO.*", ".*NOOB.*", ".*L33T.*",
                ".*MEME.*", ".*CRINGE.*", ".*SUS.*", ".*BRUH.*"
        };

        for (String pattern : memePatterns) {
            if (cleaned.matches(pattern)) {
                return cleaned + "🤡"; // Mark the meme lords
            }
        }

        // Generate random plate if too short
        if (cleaned.length() < 3) {
            return "RND" + ThreadLocalRandom.current().nextInt(100, 999);
        }

        return cleaned.length() > 8 ? cleaned.substring(0, 8) : cleaned;
    }

    // STRING MANIPULATION CHAOS
    private String sanitizeDriverName(String name) {
        if (name == null || name.trim().isEmpty()) {
            return "Anonymous_Driver_" + ThreadLocalRandom.current().nextInt(1000);
        }

        // Remove profanity (poorly)
        String cleaned = name.replaceAll("(?i)(damn|hell|crap)", "****");

        // Handle Karen detection
        if (cleaned.matches("(?i).*karen.*")) {
            return cleaned + " (MANAGER REQUESTED)";
        }

        // Handle Chad detection
        if (cleaned.matches("(?i).*(chad|brad|kyle).*")) {
            return cleaned + " (MONSTER ENERGY DETECTED)";
        }

        // Capitalize properly
        return Arrays.stream(cleaned.split("\\s+"))
                .map(word -> word.substring(0, 1).toUpperCase() +
                        word.substring(1).toLowerCase())
                .reduce((a, b) -> a + " " + b)
                .orElse("Driver McDriverface");
    }

    public abstract void drive();
    public abstract String getVehicleType();

    protected String randomHonk() {
        return honkSounds[ThreadLocalRandom.current().nextInt(honkSounds.length)];
    }

    // String formatting showcase
    public String getStatusReport() {
        StringBuilder sb = new StringBuilder();
        sb.append("═══ VEHICLE STATUS ═══\n");
        sb.append(String.format("Driver: %-20s | Plate: %s\n", driverName, licensePlate));
        sb.append(String.format("Type: %-15s | Rage: %.1f/10\n", getVehicleType(), roadRageLevel));
        sb.append(String.format("Speed Limit: %d mph | Honk: %s\n", speedLimit, randomHonk()));
        return sb.toString();
    }

    @Override
    public String toString() {
        return String.format("%s{driver='%s', plate='%s', rage=%.1f}",
                getVehicleType(), driverName, licensePlate, roadRageLevel);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || getClass() != obj.getClass()) return false;
        Vehicle vehicle = (Vehicle) obj;
        return Objects.equals(licensePlate, vehicle.licensePlate) &&
                Objects.equals(driverName, vehicle.driverName);
    }

    @Override
    public int hashCode() {
        return Objects.hash(licensePlate, driverName);
    }
}

// Karen's Minivan - Peak suburban chaos
class KarenMinivan extends Vehicle {
    private int kidsInBack;
    private boolean isLateForSoccerPractice;
    private String[] complaints;

    public KarenMinivan(String licensePlate, String driverName, int kidsInBack) {
        super(licensePlate, driverName, 8.5, "HOOOONK", "*screams at traffic*", "WHERE'S THE MANAGER");
        this.kidsInBack = Math.max(0, Math.min(8, kidsInBack));
        this.isLateForSoccerPractice = ThreadLocalRandom.current().nextBoolean();
        this.complaints = new String[]{
                "This traffic is UNACCEPTABLE",
                "I demand to speak to the mayor",
                "My kids are ANGELS",
                "I know my rights",
                "This is going on Yelp"
        };
    }

    @Override
    public void drive() {
        System.out.printf("🚐 %s drives the Karen Cruiser (plate: %s)\n", driverName, licensePlate);

        if (isLateForSoccerPractice) {
            System.out.printf("   \"%s\" *cuts through parking lot*\n",
                    complaints[ThreadLocalRandom.current().nextInt(complaints.length)]);
            System.out.printf("   Kids screaming: \"MOM ARE WE THERE YET\" x%d\n", kidsInBack);
        }

        System.out.printf("   *%s while texting*\n", randomHonk());
        System.out.println("   *blocks intersection during yellow light*");
    }

    @Override
    public String getVehicleType() {
        return "Karen Mobile";
    }
}

// Chad's Truck - Compensating for something
class ChadTruck extends Vehicle {
    private double truckHeight;
    private int monstersPerDay;
    private boolean hasNutzHanging;

    public ChadTruck(String licensePlate, String driverName, double truckHeight) {
        super(licensePlate, driverName, 9.2, "DIESEL RUMBLE", "*air horn*", "YEEHAW", "GET OUT THE WAY");
        this.truckHeight = Math.max(5, Math.min(15, truckHeight));
        this.monstersPerDay = ThreadLocalRandom.current().nextInt(3, 12);
        this.hasNutzHanging = ThreadLocalRandom.current().nextBoolean();
    }

    @Override
    public void drive() {
        System.out.printf("🛻 %s drives the Compensation Station (plate: %s)\n", driverName, licensePlate);
        System.out.printf("   Truck height: %.1f feet (%.1f feet of insecurity)\n", truckHeight, truckHeight - 5);

        if (hasNutzHanging) {
            System.out.println("   *truck nuts swaying in the wind*");
        }

        System.out.printf("   \"I've had %d Monsters today, bro!\" *%s*\n", monstersPerDay, randomHonk());
        System.out.println("   *rolls coal on Prius*");
        System.out.println("   *parks across 3 spaces*");
    }

    @Override
    public String getVehicleType() {
        return "Chad Chariot";
    }
}

// Boomer Sedan - Going exactly the speed limit
class BoomerSedan extends Vehicle {
    private boolean hasHat;
    private int newspaperSubscriptions;
    private String[] boomerComplaints;

    public BoomerSedan(String licensePlate, String driverName, boolean hasHat) {
        super(licensePlate, driverName, 2.1, "*gentle toot*", "Back in my day...", "*confused honking*");
        this.hasHat = hasHat;
        this.newspaperSubscriptions = ThreadLocalRandom.current().nextInt(1, 5);
        this.boomerComplaints = new String[]{
                "Kids these days don't know how to drive",
                "This GPS is wrong, I know a shortcut",
                "Gas was 25 cents when I was young",
                "These new cars are too complicated"
        };
    }

    @Override
    public void drive() {
        System.out.printf("🚗 %s cruises in the Time Machine (plate: %s)\n", driverName, licensePlate);

        if (hasHat) {
            System.out.println("   *hat barely visible over steering wheel*");
        }

        System.out.printf("   \"%s\"\n", boomerComplaints[ThreadLocalRandom.current().nextInt(boomerComplaints.length)]);
        System.out.printf("   *driving exactly %d mph in fast lane*\n", speedLimit - 10);
        System.out.println("   *turn signal on for 5 miles*");
    }

    @Override
    public String getVehicleType() {
        return "Boomer Boat";
    }
}

// Gen Z Car - Powered by anxiety and TikTok
class GenZCar extends Vehicle {
    private String currentVibe;
    private boolean isMainCharacter;
    private int tiktoksWatchedWhileDriving;

    public GenZCar(String licensePlate, String driverName, String currentVibe) {
        super(licensePlate, driverName, 7.8, "*aesthetic honk*", "That's so sus", "No cap this traffic is mid", "Periodt");
        this.currentVibe = currentVibe != null ? currentVibe : "chaotic neutral";
        this.isMainCharacter = ThreadLocalRandom.current().nextBoolean();
        this.tiktoksWatchedWhileDriving = ThreadLocalRandom.current().nextInt(0, 50);
    }

    @Override
    public void drive() {
        System.out.printf("🚙 %s vibes in the Anxiety Mobile (plate: %s)\n", driverName, licensePlate);
        System.out.printf("   Current vibe: %s\n", currentVibe);

        if (isMainCharacter) {
            System.out.println("   *driving like they're in a music video*");
        }

        System.out.printf("   \"Watched %d TikToks at this red light\" *%s*\n",
                tiktoksWatchedWhileDriving, randomHonk());
        System.out.println("   *takes aesthetic car selfie*");
        System.out.println("   *can't parallel park but can drift in Fortnite*");
    }

    @Override
    public String getVehicleType() {
        return "Zoomer Zoom";
    }
}

// Traffic Management System - The chaos coordinator
public class Traffic {
    private List<Vehicle> vehicles;
    private Map<String, Integer> plateFrequency;
    private Pattern speedingPattern;
    private Pattern plateValidation;

    public Traffic() {
        this.vehicles = new ArrayList<>();
        this.plateFrequency = new HashMap<>();
        this.speedingPattern = Pattern.compile(".*(?i)(fast|speed|zoom|rush).*");
        this.plateValidation = Pattern.compile("^[A-Z0-9🤡]{3,9}$");
    }

    public void addVehicle(Vehicle vehicle) {
        vehicles.add(vehicle);
        updatePlateFrequency(vehicle.licensePlate);
    }

    private void updatePlateFrequency(String plate) {
        // Remove emojis for counting
        String cleanPlate = plate.replaceAll("[🤡]", "");
        plateFrequency.put(cleanPlate, plateFrequency.getOrDefault(cleanPlate, 0) + 1);
    }

    public void startTrafficChaos() {
        System.out.println("🚦═══════════════════════════════════════════════════════════🚦");
        System.out.println("    WELCOME TO THE MEME TRAFFIC SIMULATOR 2025");
        System.out.println("    \"Where road rage meets regex and everyone loses\"");
        System.out.println("🚦═══════════════════════════════════════════════════════════🚦");

        // Vehicle roster
        System.out.println("\n📋 TODAY'S TRAFFIC DISASTERS:");
        for (int i = 0; i < vehicles.size(); i++) {
            Vehicle v = vehicles.get(i);
            System.out.printf("[%d] %s\n", i + 1, v);
            validateVehicleData(v);
        }

        // The chaos begins
        System.out.println("\n🚗💨 TRAFFIC SIMULATION STARTING...\n");
        Collections.shuffle(vehicles);

        for (Vehicle vehicle : vehicles) {
            vehicle.drive();
            analyzeDriverBehavior(vehicle);
            System.out.println("   ────────────────────────────────");
        }

        // Traffic analysis
        performTrafficAnalysis();

        // String manipulation showcase
        demonstrateStringFeatures();

        // Regex showcase
        demonstrateRegexFeatures();
    }

    private void validateVehicleData(Vehicle vehicle) {
        System.out.printf("   Plate validation: %s -> %s\n",
                vehicle.licensePlate,
                plateValidation.matcher(vehicle.licensePlate).matches() ? "✅ VALID" : "❌ INVALID");
    }

    private void analyzeDriverBehavior(Vehicle vehicle) {
        // Pattern matching for driver names
        String analysis = "";
        String name = vehicle.driverName.toLowerCase();

        if (name.matches(".*karen.*")) {
            analysis = "👩‍💼 Management consultation required";
        } else if (name.matches(".*(chad|brad|kyle).*")) {
            analysis = "💪 Protein shake levels: MAXIMUM";
        } else if (name.matches(".*(bob|robert|william).*")) {
            analysis = "👴 Boomer energy detected";
        } else if (name.matches(".*(ashley|madison|tyler).*")) {
            analysis = "📱 Main character syndrome active";
        } else {
            analysis = "🤷 Generic human behavior";
        }

        System.out.println("   Driver Analysis: " + analysis);
    }

    private void performTrafficAnalysis() {
        System.out.println("\n📊 TRAFFIC ANALYSIS REPORT:");
        System.out.println("═".repeat(50));

        // Rage level statistics
        OptionalDouble avgRage = vehicles.stream()
                .mapToDouble(v -> v.roadRageLevel)
                .average();

        Vehicle mostRaged = vehicles.stream()
                .max(Comparator.comparingDouble(v -> v.roadRageLevel))
                .orElse(null);

        System.out.printf("Average road rage: %.1f/10 (Concerning)\n", avgRage.orElse(0));
        if (mostRaged != null) {
            System.out.printf("🔥 RAGE CHAMPION: %s (%.1f rage points)\n",
                    mostRaged.driverName, mostRaged.roadRageLevel);
        }

        // License plate analysis
        System.out.println("\n🔤 LICENSE PLATE FORENSICS:");
        plateFrequency.entrySet().stream()
                .filter(entry -> entry.getValue() > 1)
                .forEach(entry -> System.out.printf("⚠️  Plate collision detected: %s appears %d times\n",
                        entry.getKey(), entry.getValue()));

        // Vehicle type distribution
        Map<String, Long> typeCount = vehicles.stream()
                .collect(java.util.stream.Collectors.groupingBy(
                        Vehicle::getVehicleType,
                        java.util.stream.Collectors.counting()
                ));

        System.out.println("\n🚗 VEHICLE TYPE BREAKDOWN:");
        typeCount.forEach((type, count) ->
                System.out.printf("%s: %d vehicle%s\n", type, count, count != 1 ? "s" : ""));
    }

    private void demonstrateStringFeatures() {
        System.out.println("\n🔤 STRING MANIPULATION SHOWCASE:");
        System.out.println("═".repeat(50));

        // Collect all driver names for analysis
        String allNames = vehicles.stream()
                .map(v -> v.driverName)
                .reduce("", (a, b) -> a + " " + b)
                .trim();

        System.out.println("📝 STRING OPERATIONS DEMO:");
        System.out.printf("All driver names: %s\n", allNames);
        System.out.printf("Total characters: %d\n", allNames.length());
        System.out.printf("Word count: %d\n", allNames.split("\\s+").length);
        System.out.printf("Uppercase: %s\n", allNames.toUpperCase());
        System.out.printf("First 50 chars: %s...\n",
                allNames.length() > 50 ? allNames.substring(0, 50) : allNames);

        // String building madness
        StringBuilder crazyString = new StringBuilder();
        crazyString.append("🚗 TRAFFIC REPORT 🚗\n");
        crazyString.append("Generated at: ").append(new Date()).append("\n");
        crazyString.append("Vehicles processed: ").append(vehicles.size()).append("\n");

        for (Vehicle v : vehicles) {
            crazyString.append(String.format("- %s drives %s\n",
                    v.driverName, v.getVehicleType()));
        }

        System.out.println("\n📄 GENERATED REPORT:");
        System.out.println(crazyString.toString());

        // String formatting showcase
        System.out.println("💅 FORMATTING SHOWCASE:");
        System.out.printf("%-20s | %-15s | %s\n", "DRIVER", "VEHICLE", "PLATE");
        System.out.println("-".repeat(55));
        for (Vehicle v : vehicles) {
            System.out.printf("%-20s | %-15s | %s\n",
                    v.driverName.length() > 20 ? v.driverName.substring(0, 17) + "..." : v.driverName,
                    v.getVehicleType(),
                    v.licensePlate);
        }
    }

    private void demonstrateRegexFeatures() {
        System.out.println("\n🔍 REGEX PATTERN MATCHING EXTRAVAGANZA:");
        System.out.println("═".repeat(50));

        // Define meme patterns
        Map<String, Pattern> patterns = new HashMap<>();
        patterns.put("Meme Lords", Pattern.compile(".*(?i)(69|420|yolo|meme|sus|bruh).*"));
        patterns.put("Karen Indicators", Pattern.compile(".*(?i)(karen|manager|speak|unacceptable).*"));
        patterns.put("Chad Signatures", Pattern.compile(".*(?i)(chad|brad|kyle|bro|monster).*"));
        patterns.put("Boomer Tells", Pattern.compile(".*(?i)(back.*day|newspaper|hat|confused).*"));
        patterns.put("Gen Z Vibes", Pattern.compile(".*(?i)(vibe|aesthetic|tiktok|periodt|cap).*"));
        patterns.put("Speed Demons", Pattern.compile(".*(?i)(fast|speed|zoom|rush|quick).*"));
        patterns.put("Profanity Filter", Pattern.compile(".*(?i)(damn|hell|crap|frick).*"));

        System.out.println("🎯 PATTERN MATCHING RESULTS:");

        for (Map.Entry<String, Pattern> entry : patterns.entrySet()) {
            String category = entry.getKey();
            Pattern pattern = entry.getValue();

            System.out.printf("\n🔎 Scanning for %s:\n", category);

            for (Vehicle vehicle : vehicles) {
                String searchText = vehicle.driverName + " " + vehicle.licensePlate + " " + vehicle.getVehicleType();

                Matcher matcher = pattern.matcher(searchText);
                if (matcher.find()) {
                    System.out.printf("   ✅ %s: \"%s\" (matched: %s)\n",
                            vehicle.driverName, searchText, matcher.group());
                }
            }
        }

        // Advanced regex showcase
        System.out.println("\n🧠 ADVANCED REGEX WIZARDRY:");

        // Email validation (even though it's a traffic sim)
        Pattern emailPattern = Pattern.compile("^[A-Za-z0-9+_.-]+@([A-Za-z0-9.-]+\\.[A-Za-z]{2,})$");
        String[] fakeEmails = {
                "karen@speakToManager.com",
                "chad.broski@monster.energy",
                "boomer1947@aol.com",
                "zoomergirl@tiktok.invalid",
                "totally.not.valid.email"
        };

        System.out.println("📧 EMAIL VALIDATION DEMO:");
        for (String email : fakeEmails) {
            System.out.printf("   %s -> %s\n", email,
                    emailPattern.matcher(email).matches() ? "✅ VALID" : "❌ INVALID");
        }

        // Phone number extraction
        Pattern phonePattern = Pattern.compile("\\b\\d{3}-\\d{3}-\\d{4}\\b");
        String messyText = "Call Karen at 555-123-4567 or Chad at 420-069-1337 for road rage counseling! Bob's number is 800-555-BOOMER but that's not a valid format.";

        System.out.println("\n📞 PHONE NUMBER EXTRACTION:");
        System.out.println("Text: " + messyText);

        Matcher phoneMatcher = phonePattern.matcher(messyText);
        System.out.println("Found phone numbers:");
        while (phoneMatcher.find()) {
            System.out.printf("   📱 %s (at position %d-%d)\n",
                    phoneMatcher.group(), phoneMatcher.start(), phoneMatcher.end());
        }

        // License plate pattern analysis
        System.out.println("\n🏷️ LICENSE PLATE PATTERN ANALYSIS:");

        Map<String, Pattern> platePatterns = new HashMap<>();
        platePatterns.put("Vanity Plates", Pattern.compile("^[A-Z]{2,8}$"));
        platePatterns.put("Standard Format", Pattern.compile("^[A-Z]{3}\\d{3,4}$"));
        platePatterns.put("Meme Plates", Pattern.compile(".*[🤡].*"));
        platePatterns.put("Numbers Only", Pattern.compile("^\\d+$"));
        platePatterns.put("Mixed Chaos", Pattern.compile("^[A-Z0-9🤡]{3,}$"));

        for (Map.Entry<String, Pattern> entry : platePatterns.entrySet()) {
            String patternName = entry.getKey();
            Pattern pattern = entry.getValue();

            long count = vehicles.stream()
                    .mapToLong(v -> pattern.matcher(v.licensePlate).matches() ? 1 : 0)
                    .sum();

            System.out.printf("   %s: %d plates\n", patternName, count);
        }

        // String replacement showcase
        System.out.println("\n🔄 STRING REPLACEMENT MADNESS:");
        String originalText = "This traffic is absolutely terrible and makes me damn angry!";

        System.out.println("Original: " + originalText);
        System.out.println("Censored: " + originalText.replaceAll("(?i)(terrible|damn)", "****"));
        System.out.println("Euphemized: " + originalText.replaceAll("(?i)(terrible|damn)", "challenging"));
        System.out.println("Meme-ified: " + originalText.replaceAll("(?i)(traffic|angry)", "sus"));

        // Split and join showcase
        String[] words = originalText.split("\\s+");
        System.out.println("Word array: " + Arrays.toString(words));
        System.out.println("Joined with |: " + String.join(" | ", words));
        System.out.println("Reversed: " + String.join(" ",
                Arrays.stream(words)
                        .collect(java.util.stream.Collectors.toList())
                        .stream()
                        .sorted(Collections.reverseOrder())
                        .toArray(String[]::new)));
    }

    // Bonus method: Generate traffic report
    public String generateTrafficReport() {
        StringBuilder report = new StringBuilder();

        report.append("🚦 DAILY TRAFFIC CHAOS REPORT 🚦\n");
        report.append("═".repeat(40)).append("\n");
        report.append("Date: ").append(new Date()).append("\n");
        report.append("Total Vehicles: ").append(vehicles.size()).append("\n\n");

        // Summary statistics
        double avgRage = vehicles.stream().mapToDouble(v -> v.roadRageLevel).average().orElse(0);
        report.append(String.format("Average Road Rage: %.1f/10\n", avgRage));

        if (avgRage > 7) {
            report.append("⚠️ CRITICAL RAGE LEVELS DETECTED\n");
        } else if (avgRage > 5) {
            report.append("⚡ MODERATE CHAOS EXPECTED\n");
        } else {
            report.append("😴 Surprisingly Peaceful (Suspicious)\n");
        }

        report.append("\n📋 INCIDENT SUMMARY:\n");
        for (Vehicle v : vehicles) {
            report.append(String.format("- %s (%s): %.1f rage points\n",
                    v.driverName, v.licensePlate, v.roadRageLevel));
        }

        report.append("\n💡 RECOMMENDATIONS:\n");
        report.append("- Avoid eye contact with Chad trucks\n");
        report.append("- Keep manager contact info handy for Karen encounters\n");
        report.append("- Bring reading material for boomer-induced delays\n");
        report.append("- Update TikTok for Gen Z traffic entertainment\n");

        return report.toString();
    }

    public static void main(String[] args) {
        Traffic chaos = new Traffic();

        // Populate the traffic nightmare
        chaos.addVehicle(new KarenMinivan("MANAGER1", "Karen McSpeak-a-lot", 4));
        chaos.addVehicle(new KarenMinivan("SOCCER69", "Karen Jr.", 2));

        chaos.addVehicle(new ChadTruck("DIESEL420", "Chad Thunderbro", 12.5));
        chaos.addVehicle(new ChadTruck("MONSTER1", "Kyle Energy-Drink", 8.7));
        chaos.addVehicle(new ChadTruck("YOLO2024", "Brad Swole-son", 15.0));

        chaos.addVehicle(new BoomerSedan("NEWS47", "Robert Newspaper", true));
        chaos.addVehicle(new BoomerSedan("AARP123", "William Confusion", false));

        chaos.addVehicle(new GenZCar("TIKTOK1", "Ashley Main-Character", "serving looks"));
        chaos.addVehicle(new GenZCar("VIBES24", "Tyler Aesthetic", "chaotic good"));
        chaos.addVehicle(new GenZCar("PERIODT", "Madison No-Cap", "unhinged energy"));

        // Add some duplicate plates for chaos
        chaos.addVehicle(new KarenMinivan("MANAGER1", "Different Karen", 3));

        // Start the beautiful disaster
        chaos.startTrafficChaos();

        // Generate final report
        System.out.println("\n" + "🚦".repeat(20));
        System.out.println("FINAL TRAFFIC REPORT");
        System.out.println("🚦".repeat(20));
        System.out.println(chaos.generateTrafficReport());

        System.out.println("🎉 SIMULATION COMPLETE!");
        System.out.println("Survivors: " + chaos.vehicles.size() + " (emotionally scarred)");
        System.out.println("Regex knowledge: ✅ ENHANCED");
        System.out.println("String manipulation skills: ✅ MAXED OUT");
        System.out.println("Will to drive: ❌ DESTROYED");

        System.out.println("\n💀 Remember: This is why we need self-driving cars!");
    }
}