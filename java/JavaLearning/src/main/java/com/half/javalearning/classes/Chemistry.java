package com.half.javalearning.classes;

import java.util.*;

// Base interfaces for chemical behavior
interface Allotrope {
    String getStructure();
    String getBonding();
    double getDensity();
    void displayProperties();
    boolean isConductor();
}

interface Combustible {
    void burn();
    String getCombustionProduct();
    double getIgnitionTemp();
}

interface LifeEssential {
    void sustainLife();
    String getBiologicalRole();
    boolean isEssentialForHumans();
}

interface IndustrialUse {
    void industrialApplication();
    double getMarketValue();
    String getMainUse();
}

// CARBON - The Chad of Elements 💎
abstract class Carbon implements Allotrope, Combustible, IndustrialUse {
    protected String name;
    protected String crystalSystem;

    public Carbon(String name, String crystalSystem) {
        this.name = name;
        this.crystalSystem = crystalSystem;
    }

    @Override
    public void burn() {
        System.out.println("🔥 " + name + " + O₂ → CO₂ + heat (and your wallet crying)");
    }

    @Override
    public String getCombustionProduct() {
        return "CO₂ (sorry, polar bears 🐻‍❄️)";
    }
}

class Diamond extends Carbon {
    public Diamond() {
        super("Diamond", "Cubic");
    }

    @Override
    public String getStructure() {
        return "💎 Tetrahedral lattice - harder than your ex's heart";
    }

    @Override
    public String getBonding() {
        return "sp³ hybridization - each carbon has 4 bestie bonds";
    }

    @Override
    public double getDensity() {
        return 3.52; // g/cm³
    }

    @Override
    public void displayProperties() {
        System.out.println("💎 DIAMOND - The Flex Allotrope");
        System.out.println("   Structure: " + getStructure());
        System.out.println("   Bonding: " + getBonding());
        System.out.println("   Density: " + getDensity() + " g/cm³");
        System.out.println("   Vibe: Expensive AF, harder than calculus");
    }

    @Override
    public boolean isConductor() {
        return false; // Electrical insulator but conducts STYLE
    }

    @Override
    public void industrialApplication() {
        System.out.println("💍 Making people broke since forever");
        System.out.println("🔪 Cutting tools that cut through EVERYTHING");
    }

    @Override
    public double getMarketValue() {
        return 50000.0; // Per carat, RIP your savings
    }

    @Override
    public String getMainUse() {
        return "Showing off and industrial cutting (priorities ✨)";
    }

    @Override
    public double getIgnitionTemp() {
        return 700.0; // °C
    }
}

class Graphite extends Carbon {
    public Graphite() {
        super("Graphite", "Hexagonal");
    }

    @Override
    public String getStructure() {
        return "📚 Layered sheets - like a really flat lasagna";
    }

    @Override
    public String getBonding() {
        return "sp² hybridization - 3 strong bonds, 1 delocalized electron (the rebel)";
    }

    @Override
    public double getDensity() {
        return 2.23; // g/cm³
    }

    @Override
    public void displayProperties() {
        System.out.println("✏️ GRAPHITE - The Chill Allotrope");
        System.out.println("   Structure: " + getStructure());
        System.out.println("   Bonding: " + getBonding());
        System.out.println("   Density: " + getDensity() + " g/cm³");
        System.out.println("   Vibe: Slippery, conductive, pencil core energy");
    }

    @Override
    public boolean isConductor() {
        return true; // Those delocalized electrons go BRRRR
    }

    @Override
    public void industrialApplication() {
        System.out.println("✏️ Pencils - helping students cheat since 1795");
        System.out.println("🔋 Batteries and electrodes");
        System.out.println("🛡️ Lubricants and heat shields");
    }

    @Override
    public double getMarketValue() {
        return 500.0; // Per ton, way more affordable than diamond
    }

    @Override
    public String getMainUse() {
        return "Writing your chemistry homework (ironic)";
    }

    @Override
    public double getIgnitionTemp() {
        return 400.0; // °C
    }
}

class Fullerene extends Carbon {
    private int carbonCount;

    public Fullerene(int carbonCount) {
        super("C" + carbonCount + " Fullerene", "Soccer ball vibes");
        this.carbonCount = carbonCount;
    }

    @Override
    public String getStructure() {
        return "⚽ Cage-like structure - literally a soccer ball made of carbon";
    }

    @Override
    public String getBonding() {
        return "Mixed sp² and sp³ - identity crisis bonding";
    }

    @Override
    public double getDensity() {
        return 1.65; // g/cm³
    }

    @Override
    public void displayProperties() {
        System.out.println("⚽ FULLERENE (C" + carbonCount + ") - The Weird Allotrope");
        System.out.println("   Structure: " + getStructure());
        System.out.println("   Bonding: " + getBonding());
        System.out.println("   Density: " + getDensity() + " g/cm³");
        System.out.println("   Vibe: Molecular cage, traps smaller molecules");
    }

    @Override
    public boolean isConductor() {
        return false; // But can become superconductor when doped 🤯
    }

    @Override
    public void industrialApplication() {
        System.out.println("💊 Drug delivery systems");
        System.out.println("🧴 Cosmetics and lubricants");
        System.out.println("🔬 Nanotechnology research");
    }

    @Override
    public double getMarketValue() {
        return 15000.0; // Per gram, research grade expensive
    }

    @Override
    public String getMainUse() {
        return "Making chemists feel smart at conferences";
    }

    @Override
    public double getIgnitionTemp() {
        return 600.0; // °C
    }
}

// OXYGEN - The Life Support System 🫁
abstract class Oxygen implements Allotrope, LifeEssential, Combustible {
    protected String formula;

    public Oxygen(String formula) {
        this.formula = formula;
    }

    @Override
    public void sustainLife() {
        System.out.println("🫁 Keeping you alive since birth (you're welcome)");
    }

    @Override
    public boolean isEssentialForHumans() {
        return true; // Duh
    }
}

class Dioxygen extends Oxygen {
    public Dioxygen() {
        super("O₂");
    }

    @Override
    public String getStructure() {
        return "🔗 Linear diatomic molecule - two oxygen atoms being besties";
    }

    @Override
    public String getBonding() {
        return "Double bond with two unpaired electrons (paramagnetic king)";
    }

    @Override
    public double getDensity() {
        return 1.429; // g/L at STP
    }

    @Override
    public void displayProperties() {
        System.out.println("🫁 DIOXYGEN (O₂) - The Life Giver");
        System.out.println("   Structure: " + getStructure());
        System.out.println("   Bonding: " + getBonding());
        System.out.println("   Density: " + getDensity() + " g/L");
        System.out.println("   Vibe: Essential for breathing, slightly magnetic");
    }

    @Override
    public boolean isConductor() {
        return false; // Insulator in gas form
    }

    @Override
    public String getBiologicalRole() {
        return "Cellular respiration MVP, literally keeps you alive";
    }

    @Override
    public void burn() {
        System.out.println("🔥 Supports combustion - the hype man of fire");
    }

    @Override
    public String getCombustionProduct() {
        return "Makes other things burn (it's the enabler)";
    }

    @Override
    public double getIgnitionTemp() {
        return 0.0; // Doesn't ignite, but makes everything else ignite
    }
}

class Ozone extends Oxygen {
    public Ozone() {
        super("O₃");
    }

    @Override
    public String getStructure() {
        return "🌀 Bent triatomic molecule - like O₂ but with commitment issues";
    }

    @Override
    public String getBonding() {
        return "Resonance structure - can't decide which bond to make";
    }

    @Override
    public double getDensity() {
        return 2.144; // g/L at STP
    }

    @Override
    public void displayProperties() {
        System.out.println("🛡️ OZONE (O₃) - The Protective Layer");
        System.out.println("   Structure: " + getStructure());
        System.out.println("   Bonding: " + getBonding());
        System.out.println("   Density: " + getDensity() + " g/L");
        System.out.println("   Vibe: UV shield, smells like after a thunderstorm");
    }

    @Override
    public boolean isConductor() {
        return false;
    }

    @Override
    public String getBiologicalRole() {
        return "Ozone layer protects us from UV rays (environmental bodyguard)";
    }

    @Override
    public void burn() {
        System.out.println("💥 Decomposes violently - anger management issues");
    }

    @Override
    public String getCombustionProduct() {
        return "Breaks down to O₂ (anger management success)";
    }

    @Override
    public double getIgnitionTemp() {
        return -112.0; // °C, very unstable
    }
}

// NITROGEN - The Chill Gas 😴
abstract class Nitrogen implements Allotrope, LifeEssential, IndustrialUse {
    protected String state;

    public Nitrogen(String state) {
        this.state = state;
    }

    @Override
    public void sustainLife() {
        System.out.println("🧬 Part of amino acids and DNA - literally in your genes");
    }

    @Override
    public boolean isEssentialForHumans() {
        return true; // Proteins need it
    }
}

class Dinitrogen extends Nitrogen {
    public Dinitrogen() {
        super("Gaseous");
    }

    @Override
    public String getStructure() {
        return "💤 Linear diatomic molecule - the sleepy gas";
    }

    @Override
    public String getBonding() {
        return "Triple bond - strongest bond in the game, very unreactive";
    }

    @Override
    public double getDensity() {
        return 1.251; // g/L at STP
    }

    @Override
    public void displayProperties() {
        System.out.println("💤 DINITROGEN (N₂) - The Inert Majority");
        System.out.println("   Structure: " + getStructure());
        System.out.println("   Bonding: " + getBonding());
        System.out.println("   Density: " + getDensity() + " g/L");
        System.out.println("   Vibe: Makes up 78% of air, but does nothing");
    }

    @Override
    public boolean isConductor() {
        return false; // Very unreactive
    }

    @Override
    public String getBiologicalRole() {
        return "Nitrogen cycle, protein synthesis (the behind-the-scenes worker)";
    }

    @Override
    public void industrialApplication() {
        System.out.println("❄️ Liquid nitrogen for cooling");
        System.out.println("🥤 Inert atmosphere for food packaging");
        System.out.println("💣 Fertilizers and explosives (dual personality)");
    }

    @Override
    public double getMarketValue() {
        return 0.10; // Per cubic meter, dirt cheap
    }

    @Override
    public String getMainUse() {
        return "Filling up space in the atmosphere and cooling things down";
    }
}

class SolidNitrogen extends Nitrogen {
    public SolidNitrogen() {
        super("Solid");
    }

    @Override
    public String getStructure() {
        return "❄️ Face-centered cubic crystal - frozen N₂ molecules";
    }

    @Override
    public String getBonding() {
        return "Van der Waals forces between N₂ molecules";
    }

    @Override
    public double getDensity() {
        return 1.026; // g/cm³ at -210°C
    }

    @Override
    public void displayProperties() {
        System.out.println("🧊 SOLID NITROGEN - The Frozen State");
        System.out.println("   Structure: " + getStructure());
        System.out.println("   Bonding: " + getBonding());
        System.out.println("   Density: " + getDensity() + " g/cm³");
        System.out.println("   Vibe: Exists only at very low temps");
    }

    @Override
    public boolean isConductor() {
        return false;
    }

    @Override
    public String getBiologicalRole() {
        return "Not really biological at these temps (everything's dead)";
    }

    @Override
    public void industrialApplication() {
        System.out.println("🧪 Cryogenic research");
        System.out.println("❄️ Extreme cooling applications");
    }

    @Override
    public double getMarketValue() {
        return 5.0; // Per kg, specialty application
    }

    @Override
    public String getMainUse() {
        return "Making things REALLY cold";
    }
}

// The Grand Chemistry Simulator
public class Chemistry {
    public static void main(String[] args) {
        System.out.println("🧪 WELCOME TO THE ALLOTROPY LABORATORY 🧪");
        System.out.println("Where chemistry meets chaos and Java meets periodic table! 💀\n");

        // Carbon Squad Assembly 💎
        System.out.println("═══════════════════════════════════════");
        System.out.println("🔥 CARBON GANG - THE SHAPE SHIFTERS 🔥");
        System.out.println("═══════════════════════════════════════");

        Diamond diamond = new Diamond();
        Graphite graphite = new Graphite();
        Fullerene buckyball = new Fullerene(60);

        diamond.displayProperties();
        System.out.println();
        graphite.displayProperties();
        System.out.println();
        buckyball.displayProperties();

        System.out.println("\n🔥 CARBON COMBUSTION TEST:");
        diamond.burn();
        graphite.burn();
        buckyball.burn();

        System.out.println("\n💰 CARBON MARKET ANALYSIS:");
        System.out.printf("💎 Diamond: $%.2f per carat (bankruptcy incoming)\n", diamond.getMarketValue());
        System.out.printf("✏️ Graphite: $%.2f per ton (actually affordable)\n", graphite.getMarketValue());
        System.out.printf("⚽ Fullerene: $%.2f per gram (research flex)\n", buckyball.getMarketValue());

        // Oxygen Squad Assembly 🫁
        System.out.println("\n═══════════════════════════════════════");
        System.out.println("🫁 OXYGEN CREW - THE LIFE SUPPORTERS 🫁");
        System.out.println("═══════════════════════════════════════");

        Dioxygen oxygen = new Dioxygen();
        Ozone ozone = new Ozone();

        oxygen.displayProperties();
        System.out.println();
        ozone.displayProperties();

        System.out.println("\n🫁 LIFE SUPPORT TEST:");
        oxygen.sustainLife();
        ozone.sustainLife();

        System.out.println("\n🔥 OXYGEN COMBUSTION BEHAVIOR:");
        oxygen.burn();
        ozone.burn();

        // Nitrogen Squad Assembly 💤
        System.out.println("\n═══════════════════════════════════════");
        System.out.println("💤 NITROGEN FAMILY - THE CHILL ONES 💤");
        System.out.println("═══════════════════════════════════════");

        Dinitrogen nitrogen = new Dinitrogen();
        SolidNitrogen frozenNitrogen = new SolidNitrogen();

        nitrogen.displayProperties();
        System.out.println();
        frozenNitrogen.displayProperties();

        System.out.println("\n🧬 NITROGEN LIFE SUPPORT:");
        nitrogen.sustainLife();
        frozenNitrogen.sustainLife();

        System.out.println("\n🏭 INDUSTRIAL APPLICATIONS:");
        nitrogen.industrialApplication();
        System.out.println();
        frozenNitrogen.industrialApplication();

        // ULTIMATE ALLOTROPE BATTLE ROYALE 🥊
        System.out.println("\n═══════════════════════════════════════");
        System.out.println("🥊 ALLOTROPE BATTLE ROYALE 🥊");
        System.out.println("═══════════════════════════════════════");

        List<Allotrope> allotropes = Arrays.asList(
                diamond, graphite, buckyball, oxygen, ozone, nitrogen, frozenNitrogen
        );

        System.out.println("🏆 CONDUCTIVITY CHAMPIONSHIP:");
        allotropes.forEach(allotrope -> {
            String name = allotrope.getClass().getSimpleName();
            String status = allotrope.isConductor() ? "⚡ CONDUCTIVE" : "🚫 INSULATOR";
            System.out.println("   " + name + ": " + status);
        });

        System.out.println("\n📊 DENSITY RANKINGS (heaviest to lightest):");
        allotropes.stream()
                .sorted((a, b) -> Double.compare(b.getDensity(), a.getDensity()))
                .forEach(allotrope -> {
                    String name = allotrope.getClass().getSimpleName();
                    System.out.printf("   %s: %.3f g/cm³ or g/L\n", name, allotrope.getDensity());
                });

        // Chemistry Lab Chaos Simulator 🧪
        System.out.println("\n═══════════════════════════════════════");
        System.out.println("🧪 CHEMISTRY LAB CHAOS SIMULATOR 🧪");
        System.out.println("═══════════════════════════════════════");

        ChemistryExperiment experiment = new ChemistryExperiment();
        experiment.mixRandomElements(allotropes);

        System.out.println("\n🎓 CHEMISTRY FACTS THAT WILL BLOW YOUR MIND:");
        System.out.println("💎 Diamond and graphite are both pure carbon but diamond costs 100x more");
        System.out.println("⚽ Fullerenes were discovered by accident and won a Nobel Prize");
        System.out.println("🛡️ Without ozone layer, you'd be crispy in minutes");
        System.out.println("💤 78% of air is nitrogen but it's too lazy to react with anything");
        System.out.println("🔥 Oxygen doesn't burn, it just makes everything else burn better");

        System.out.println("\n🏁 EXPERIMENT COMPLETE!");
        System.out.println("Your chemistry teacher would be proud... or terrified 😈");
    }
}

// Bonus: Chemistry Experiment Simulator
class ChemistryExperiment {
    private Random random = new Random();

    public void mixRandomElements(List<Allotrope> allotropes) {
        System.out.println("🥽 Putting on safety goggles...");
        System.out.println("🧤 Wearing lab gloves...");
        System.out.println("👨‍🔬 Starting random chemical experiment!");

        // Pick two random allotropes
        Allotrope element1 = allotropes.get(random.nextInt(allotropes.size()));
        Allotrope element2 = allotropes.get(random.nextInt(allotropes.size()));

        String name1 = element1.getClass().getSimpleName();
        String name2 = element2.getClass().getSimpleName();

        System.out.println("\n🧪 Mixing " + name1 + " + " + name2 + "...");

        // Simulate reaction based on properties
        if (element1.isConductor() && element2.isConductor()) {
            System.out.println("⚡ BZZZZT! Electrical reaction! Sparks flying!");
        } else if (name1.contains("Oxygen") || name2.contains("Oxygen")) {
            System.out.println("🔥 WHOOSH! Combustion reaction! Fire everywhere!");
        } else if (name1.contains("Nitrogen") || name2.contains("Nitrogen")) {
            System.out.println("💤 *yawn* Nothing happened. Nitrogen is too chill.");
        } else {
            System.out.println("💥 BOOM! Unknown reaction! Evacuate the lab!");
        }

        // Random lab accident
        String[] accidents = {
                "🧪 Beaker exploded! Glass everywhere!",
                "👨‍🔬 Lab coat caught fire! Stop, drop, and roll!",
                "💨 Toxic gas leak! Everyone out!",
                "🌈 Pretty colors appeared! Probably shouldn't inhale...",
                "❄️ Everything froze solid! Winter is coming!",
                "✨ Magical sparkles! This isn't chemistry anymore...",
                "🦠 Something mutated! Call the biohazard team!",
                "🕳️ Burned a hole through the table! Oops..."
        };

        System.out.println("🚨 LAB INCIDENT: " + accidents[random.nextInt(accidents.length)]);
        System.out.println("📝 Writing incident report... again...");
    }
}

// Bonus Interface: Toxic Behavior (because chemistry can be dangerous)
interface Toxic {
    void poisonEverything();
    String getAntidote();
    int getToxicityLevel(); // 1-10 scale
}

// Bonus: Toxic Nitrogen Compounds (because why not add more chaos)
class NitrogenDioxide extends Nitrogen implements Toxic {
    public NitrogenDioxide() {
        super("Gaseous Death");
    }

    @Override
    public String getStructure() {
        return "💀 Bent molecule with unpaired electron - angry and reactive";
    }

    @Override
    public String getBonding() {
        return "Resonance between single and double bonds (can't make up its mind)";
    }

    @Override
    public double getDensity() {
        return 1.88; // g/L
    }

    @Override
    public void displayProperties() {
        System.out.println("💀 NITROGEN DIOXIDE (NO₂) - The Toxic One");
        System.out.println("   Structure: " + getStructure());
        System.out.println("   Bonding: " + getBonding());
        System.out.println("   Density: " + getDensity() + " g/L");
        System.out.println("   Vibe: Brown gas of death, acid rain contributor");
    }

    @Override
    public boolean isConductor() {
        return false;
    }

    @Override
    public String getBiologicalRole() {
        return "Lung irritant and environmental destroyer";
    }

    @Override
    public void industrialApplication() {
        System.out.println("🏭 Nitric acid production");
        System.out.println("💣 Rocket propellant oxidizer");
        System.out.println("🚗 Unwanted car exhaust byproduct");
    }

    @Override
    public double getMarketValue() {
        return -1000.0; // Negative value because it's a pollutant
    }

    @Override
    public String getMainUse() {
        return "Making environmentalists cry";
    }

    @Override
    public void poisonEverything() {
        System.out.println("☠️ Destroying lung tissue one breath at a time!");
        System.out.println("🌧️ Contributing to acid rain!");
        System.out.println("🌍 General environmental villainy!");
    }

    @Override
    public String getAntidote() {
        return "Fresh air and staying away from busy roads";
    }

    @Override
    public int getToxicityLevel() {
        return 8; // Very toxic
    }
}