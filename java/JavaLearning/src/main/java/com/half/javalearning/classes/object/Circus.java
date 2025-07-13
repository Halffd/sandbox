package com.half.javalearning.classes.object;

import java.util.*;
import java.util.concurrent.ThreadLocalRandom;

// Base circus entity - because everyone's a clown here
abstract class CircusEntity {
    protected String name;
    protected int age;
    protected double cringe_level;
    protected String[] catchphrases;

    public CircusEntity(String name, int age, double cringe_level, String... catchphrases) {
        this.name = name;
        this.age = age;
        this.cringe_level = Math.max(0, Math.min(11, cringe_level)); // it goes to 11
        this.catchphrases = catchphrases.length > 0 ? catchphrases : new String[]{"..."};
    }

    public abstract void performAct();
    public abstract String getVibeCheck();

    protected String randomCatchphrase() {
        return catchphrases[ThreadLocalRandom.current().nextInt(catchphrases.length)];
    }

    @Override
    public String toString() {
        return String.format("%s{name='%s', age=%d, cringe=%.1f}",
                getClass().getSimpleName(), name, age, cringe_level);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || getClass() != obj.getClass()) return false;
        CircusEntity that = (CircusEntity) obj;
        return age == that.age &&
                Double.compare(that.cringe_level, cringe_level) == 0 &&
                Objects.equals(name, that.name);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name, age, cringe_level);
    }
}

// Alpha kids - Gen Z circus performers (help us all)
class AlphaKid extends CircusEntity {
    private String favoriteApp;
    private int tiktokFollowers;
    private boolean isInfluencer;
    private String[] brainrotWords = {"skibidi", "sigma", "rizz", "ohio", "gyatt", "mewing"};

    public AlphaKid(String name, int age, String favoriteApp, int tiktokFollowers) {
        super(name, age, 10.5, "No cap fr fr", "That's so sigma", "Skill issue tbh", "Touch grass");
        this.favoriteApp = favoriteApp;
        this.tiktokFollowers = tiktokFollowers;
        this.isInfluencer = tiktokFollowers > 1000;
    }

    @Override
    public void performAct() {
        String brainrot = brainrotWords[ThreadLocalRandom.current().nextInt(brainrotWords.length)];
        System.out.printf("🎪 %s (age %d) starts their act:\n", name, age);
        System.out.printf("   *does %s dance while saying '%s'*\n", brainrot, randomCatchphrase());
        System.out.printf("   \"Don't forget to like and subscribe! I have %d followers on %s!\"\n",
                tiktokFollowers, favoriteApp);
        System.out.println("   *crowd of boomers confused*");
    }

    @Override
    public String getVibeCheck() {
        if (isInfluencer) return "💎 INFLUENCER VIBES - Built different";
        if (tiktokFollowers > 100) return "📱 Main character energy";
        return "👶 Smol bean trying their best";
    }

    @Override
    public String toString() {
        return String.format("AlphaKid{name='%s', age=%d, app='%s', followers=%d, influencer=%s}",
                name, age, favoriteApp, tiktokFollowers, isInfluencer);
    }
}

// Animals - the real MVPs of this circus
class CircusAnimal extends CircusEntity {
    private String species;
    private String specialTrick;
    private boolean isDiva;
    private int treatsDemanded;

    public CircusAnimal(String name, int age, String species, String specialTrick, boolean isDiva) {
        super(name, age, ThreadLocalRandom.current().nextDouble(1, 8),
                "*animal noises*", "*stares judgmentally*", "*demands treats*");
        this.species = species;
        this.specialTrick = specialTrick;
        this.isDiva = isDiva;
        this.treatsDemanded = ThreadLocalRandom.current().nextInt(1, 20);
    }

    @Override
    public void performAct() {
        System.out.printf("🎪 %s the %s enters the ring:\n", name, species);

        if (isDiva) {
            System.out.printf("   *%s refuses to perform until getting %d treats*\n", name, treatsDemanded);
            System.out.println("   *trainer sweating profusely*");
        }

        System.out.printf("   *performs %s*\n", specialTrick);
        System.out.printf("   \"%s\"\n", randomCatchphrase());

        if (ThreadLocalRandom.current().nextBoolean()) {
            System.out.println("   *nails the performance like a boss*");
        } else {
            System.out.println("   *completely ignores commands and does own thing*");
        }
    }

    @Override
    public String getVibeCheck() {
        if (isDiva) return "👑 DIVA ENERGY - Demands respect";
        if (treatsDemanded > 15) return "🍖 SNACK GOBLIN - Will work for food";
        return "😇 Good boi/girl energy";
    }

    @Override
    public String toString() {
        return String.format("CircusAnimal{name='%s', species='%s', trick='%s', diva=%s}",
                name, species, specialTrick, isDiva);
    }
}

// Clowns - the chaotic evil of the circus
class Clown extends CircusEntity {
    private String clownType;
    private boolean isActuallyFunny;
    private int dadJokesKnown;
    private String[] props;

    public Clown(String name, int age, String clownType, boolean isActuallyFunny, String... props) {
        super(name, age, isActuallyFunny ? 3.0 : 9.5,
                "HONK HONK", "Why so serious?", "That's a knee-slapper!", "Ba dum tss");
        this.clownType = clownType;
        this.isActuallyFunny = isActuallyFunny;
        this.dadJokesKnown = ThreadLocalRandom.current().nextInt(10, 500);
        this.props = props.length > 0 ? props : new String[]{"rubber chicken", "whoopee cushion"};
    }

    @Override
    public void performAct() {
        System.out.printf("🎪 %s the %s clown waddles into the spotlight:\n", name, clownType);

        String prop = props[ThreadLocalRandom.current().nextInt(props.length)];
        System.out.printf("   *pulls out %s*\n", prop);

        if (isActuallyFunny) {
            System.out.println("   *delivers genuinely hilarious routine*");
            System.out.println("   *audience actually laughing*");
        } else {
            System.out.printf("   \"I know %d dad jokes... and they're all terrible!\"\n", dadJokesKnown);
            System.out.println("   *cricket sounds*");
            System.out.println("   *one kid crying*");
        }

        System.out.printf("   \"%s\"\n", randomCatchphrase());
    }

    @Override
    public String getVibeCheck() {
        if (isActuallyFunny) return "😂 COMEDY GOLD - Actually funny";
        if (dadJokesKnown > 200) return "🤡 WALKING CRINGE - Weapon of dad jokes";
        return "😬 Trying their best but... yikes";
    }

    @Override
    public String toString() {
        return String.format("Clown{name='%s', type='%s', funny=%s, dadJokes=%d}",
                name, clownType, isActuallyFunny, dadJokesKnown);
    }
}

// The main circus - where dreams go to die
public class Circus {
    private List<CircusEntity> performers;
    private String circusName;
    private double overallChaosLevel;

    public Circus(String name) {
        this.circusName = name;
        this.performers = new ArrayList<>();
        this.overallChaosLevel = 0.0;
    }

    public void addPerformer(CircusEntity performer) {
        performers.add(performer);
        overallChaosLevel += performer.cringe_level;
    }

    public void startShow() {
        System.out.println("🎪═══════════════════════════════════════════════════════════🎪");
        System.out.printf("    WELCOME TO %s\n", circusName.toUpperCase());
        System.out.printf("    Chaos Level: %.1f/10 (%.1f overall)\n",
                overallChaosLevel / performers.size(), overallChaosLevel);
        System.out.println("    \"Where logic comes to die and memes are born\"\n");
        System.out.println("🎪═══════════════════════════════════════════════════════════🎪");

        // Cast introduction
        System.out.println("\n📋 TONIGHT'S CAST OF CHAOS:");
        for (int i = 0; i < performers.size(); i++) {
            CircusEntity performer = performers.get(i);
            System.out.printf("[%d] %s - %s\n", i + 1, performer, performer.getVibeCheck());
        }

        // The actual show
        System.out.println("\n🎭 LET THE CHAOS BEGIN!\n");
        Collections.shuffle(performers); // Random order for maximum chaos

        for (CircusEntity performer : performers) {
            performer.performAct();
            System.out.println("   " + performer.getVibeCheck());
            System.out.println("   ────────────────────────────────");

            // Random audience reactions
            String[] reactions = {
                    "*confused applause*", "*someone's phone rings*", "*kid asks for bathroom*",
                    "*boomer doesn't get it*", "*gen z living for this*", "*millennial having flashbacks*",
                    "*karen demands refund*", "*actually enjoying the show*"
            };
            System.out.println("   Audience: " + reactions[ThreadLocalRandom.current().nextInt(reactions.length)]);
            System.out.println();
        }

        // Show statistics
        showStats();

        // Final chaos
        System.out.println("🎪 SHOW'S OVER! Everyone's emotionally scarred but somehow entertained!");
        System.out.println("🎪 Don't forget to tip your performers (they need therapy money)!");
    }

    private void showStats() {
        System.out.println("📊 POST-SHOW STATISTICS:");

        // Group by type
        Map<String, List<CircusEntity>> groups = new HashMap<>();
        for (CircusEntity performer : performers) {
            String type = performer.getClass().getSimpleName();
            groups.computeIfAbsent(type, k -> new ArrayList<>()).add(performer);
        }

        groups.forEach((type, list) -> {
            double avgCringe = list.stream().mapToDouble(p -> p.cringe_level).average().orElse(0);
            System.out.printf("%s: %d performer%s (avg cringe: %.1f)\n",
                    type, list.size(), list.size() != 1 ? "s" : "", avgCringe);
        });

        // Find the most cringe
        CircusEntity mostCringe = performers.stream()
                .max(Comparator.comparingDouble(p -> p.cringe_level))
                .orElse(null);

        if (mostCringe != null) {
            System.out.printf("🏆 CRINGE CHAMPION: %s (%.1f cringe points)\n",
                    mostCringe.name, mostCringe.cringe_level);
        }

        // Test equals and hashCode
        System.out.println("\n🔍 DUPLICATE DETECTION:");
        boolean foundDupes = false;
        for (int i = 0; i < performers.size(); i++) {
            for (int j = i + 1; j < performers.size(); j++) {
                if (performers.get(i).equals(performers.get(j))) {
                    System.out.printf("CLONE DETECTED: %s and %s are literally the same person!\n",
                            performers.get(i).name, performers.get(j).name);
                    foundDupes = true;
                }
            }
        }
        if (!foundDupes) {
            System.out.println("No clones detected - everyone's uniquely cursed!");
        }

        // Hash codes for the nerds
        System.out.println("\n#️⃣ HASH CODES (for debugging your life choices):");
        performers.forEach(p -> System.out.printf("%s: %d\n", p.name, p.hashCode()));
    }

    public static void main(String[] args) {
        Circus circus = new Circus("The Brainrot Bonanza Circus");

        // Add the chaos crew
        circus.addPerformer(new AlphaKid("Kayden", 12, "TikTok", 50000));
        circus.addPerformer(new AlphaKid("Brayden", 10, "YouTube Shorts", 500));
        circus.addPerformer(new AlphaKid("Aiden", 14, "Instagram", 2000));

        circus.addPerformer(new CircusAnimal("Mr. Whiskers", 5, "Cat", "backflips while judging humans", true));
        circus.addPerformer(new CircusAnimal("Doggo McBork", 3, "Dog", "catches frisbees and hearts", false));
        circus.addPerformer(new CircusAnimal("Chonky Boi", 8, "Elephant", "remembers your embarrassing moments", true));
        circus.addPerformer(new CircusAnimal("Slithery Steve", 2, "Snake", "hypnotizes audience with sick moves", false));

        circus.addPerformer(new Clown("Pennywise Jr.", 35, "Nightmare Fuel", false, "balloon animals", "existential dread"));
        circus.addPerformer(new Clown("Bobo the Blessed", 28, "Actually Funny", true, "rubber chicken", "whoopee cushion", "comedy gold"));
        circus.addPerformer(new Clown("Dad Joke Dave", 45, "Suburban Dad", false, "dad jokes", "lawn care tips"));
        // Duplicate for testing equals
        circus.addPerformer(new AlphaKid("Kayden", 12, "TikTok", 50000)); // Same as first one

        // Start the absolute chaos
        circus.startShow();

        System.out.println("\n" + "=".repeat(60));
        System.out.println("BONUS ROUND: OBJECT METHODS DEEP DIVE");
        System.out.println("=".repeat(60));

        // Advanced Object method demonstrations
        CircusEntity testSubject = circus.performers.get(0);

        System.out.println("\n🔬 OBJECT AUTOPSY:");
        System.out.println("Subject: " + testSubject.name);
        System.out.println("getClass(): " + testSubject.getClass());
        System.out.println("getClass().getName(): " + testSubject.getClass().getName());
        System.out.println("getClass().getSimpleName(): " + testSubject.getClass().getSimpleName());
        System.out.println("getClass().getSuperclass(): " + testSubject.getClass().getSuperclass());
        System.out.println("toString(): " + testSubject.toString());
        System.out.println("hashCode(): " + testSubject.hashCode());

        // instanceof checks
        System.out.println("\n🏷️ IDENTITY CRISIS CHECK:");
        System.out.printf("%s instanceof CircusEntity: %s\n", testSubject.name, testSubject instanceof CircusEntity);
        System.out.printf("%s instanceof AlphaKid: %s\n", testSubject.name, testSubject instanceof AlphaKid);
        System.out.printf("%s instanceof CircusAnimal: %s\n", testSubject.name, testSubject instanceof CircusAnimal);
        System.out.printf("%s instanceof Clown: %s\n", testSubject.name, testSubject instanceof Clown);
        System.out.printf("%s instanceof Object: %s (spoiler: always true)\n", testSubject.name, testSubject instanceof Object);

        // Polymorphism showcase
        System.out.println("\n🎭 POLYMORPHISM PARTY:");
        CircusEntity[] polyArray = {
                new AlphaKid("Zoomer Zoe", 13, "Snapchat", 10000),
                new CircusAnimal("Philosophy Cat", 4, "Cat", "contemplates existence", true),
                new Clown("Meme Lord Mike", 30, "Internet Clown", true, "dank memes", "cringe compilation")
        };

        for (CircusEntity entity : polyArray) {
            System.out.println("\nRuntime type: " + entity.getClass().getSimpleName());
            entity.performAct();
            System.out.println("Vibe: " + entity.getVibeCheck());
        }

        // Collection shenanigans
        System.out.println("\n📦 COLLECTION CHAOS:");
        Set<CircusEntity> uniquePerformers = new HashSet<>(circus.performers);
        System.out.printf("Original performers: %d\n", circus.performers.size());
        System.out.printf("Unique performers (HashSet): %d\n", uniquePerformers.size());
        System.out.println("Difference shows our equals/hashCode is working!");

        // Final meme moment
        System.out.println("\n💀 FINAL BOSS: CURSED OBJECT METHODS");

        // Create cursed performer for maximum chaos
        AlphaKid cursedChild = new AlphaKid("X Æ A-XII", 8, "Neuralink", 999999) {
            @Override
            public String toString() {
                return "🤖 CURSED_ENTITY{error=404_childhood_not_found}";
            }

            @Override
            public void performAct() {
                System.out.println("🤖 *speaks in binary*");
                System.out.println("   01001000 01100101 01101100 01110000");
                System.out.println("   Translation: 'Help, I'm trapped in a Java example'");
                System.out.println("   *glitches out of existence*");
            }

            @Override
            public String getVibeCheck() {
                return "🤖 GLITCHED - Exists in quantum superposition of cringe";
            }
        };

        System.out.println("\nBehold, the final boss:");
        cursedChild.performAct();
        System.out.println("toString(): " + cursedChild.toString());
        System.out.println("hashCode(): " + cursedChild.hashCode());
        System.out.println("Class: " + cursedChild.getClass().getSuperclass().getSimpleName() + " (but cursed)");

        // The end
        System.out.println("\n" + "🎪".repeat(20));
        System.out.println("THAT'S ALL FOLKS! 🎭");
        System.out.println("You've survived the Circus of Object Methods!");
        System.out.println("Your sanity: DEPRECATED ❌");
        System.out.println("Your Java knowledge: ENHANCED ✅");
        System.out.println("Your will to live: QUESTIONABLE ❓");
        System.out.println("🎪".repeat(20));
    }
}