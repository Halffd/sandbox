package com.half.javalearning.person;

import java.util.*;

/**

 * X-Men: Anomaly Generator
 * Creates random mutants with wild statistical variations
 **/
 class XMenAnomalyGenerator {

 public enum PowerLevel { OMEGA, ALPHA, BETA, GAMMA, DELTA }

 // Name components for random generation
 private static final String[] FIRST_NAMES = {
 "Xe", "Tor", "Kra", "Zy", "Mu", "Vor", "Qi", "Aza", "Nyx", "Elek",
 "Bla", "Psy", "Hex", "Evo", "Bio", "Tek", "Fus", "Ril", "Dyn", "Zeph"
 };

 private static final String[] LAST_NAMES = {
 "tron", "zor", "nex", "plex", "mancer", "flux", "glow", "pulse", "phase", "vex",
 "wraith", "forge", "core", "storm", "blade", "mind", "fire", "wave", "null", "spark"
 };

 // Codename prefixes and suffixes
 private static final String[] CODE_PREFIXES = {
 "PHASE", "SHADOW", "OMEGA", "HYPER", "CRYO", "PYRO", "TECHNO", "PSI", "QUANTUM", "COSMIC",
 "VECTOR", "PRIME", "ALPHA", "VOID", "FLUX", "STORM", "TITAN", "MEGA", "ULTRA", "ZERO"
 };

 private static final String[] CODE_SUFFIXES = {
 "BLADE", "STRIKE", "FORCE", "SHIFT", "SIGHT", "GRIP", "MIND", "FIST", "WAVE", "PULSE",
 "DRIFT", "STEP", "FLUX", "BLAST", "SHOCK", "FORM", "LINK", "GATE", "PATH", "SPARK"
 };

 // Powers for random selection
 private static final String[] POWERS = {
 "Reality Manipulation", "Telekinesis", "Telepathy", "Time Distortion", "Elemental Control",
 "Energy Absorption", "Phasing", "Super Strength", "Healing Factor", "Shapeshifting",
 "Teleportation", "Force Field Generation", "Precognition", "Energy Projection", "Matter Transmutation",
 "Technopathy", "Gravity Control", "Illusion Casting", "Duplication", "Empathic Manipulation"
 };

 // Weaknesses for random selection
 private static final String[] WEAKNESSES = {
 "Power burnout after extended use", "Vulnerability to specific radiation", "Mental feedback loop",
 "Physical exhaustion", "Limited range", "Unstable molecular structure", "Emotional triggers",
 "Sensory overload", "Power nullification fields", "Specific sound frequencies",
 "Temporal displacement sickness", "Energy depletion", "Psychic vulnerability", "Magnetic disruption",
 "Extreme temperatures", "Chemical suppressants", "Solar dependency", "Interdimensional feedback",
 "Control limitations", "Power inversion under stress"
 };

 // Team names
 private static final String[] TEAMS = {
 "X-Men", "X-Force", "New Mutants", "Excalibur", "X-Factor"
 };

 public static class Mutant {
 private String realName;
 private String codeName;
 private double powerReserve; // Money represents mutant energy
 private int chronologicalAge;
 private int actualAge; // Some mutants age differently
 private char gender;
 private double height;
 private double mutantGeneSequence; // ID
 private List<String> powers;
 private PowerLevel classification;
 private String team;
 private String weakness;

 public Mutant(String realName, String codeName, double powerReserve,
 int age, char gender, double height, double geneSequence) {
 this.realName = realName;
 this.codeName = codeName;
 this.powerReserve = powerReserve;
 this.chronologicalAge = age;
 this.actualAge = age;
 this.gender = gender;
 this.height = height;
 this.mutantGeneSequence = geneSequence;
 this.powers = new ArrayList<>();
 this.team = "X-Men";
 }

 public void activateMutantAbility(String power) {
 if (powers.contains(power)) {
 System.out.println("█ " + codeName + " activates: " + power.toUpperCase() + "!");

 if (power.contains("Reality") || power.contains("Time")) {
 System.out.println("  └─ The fabric of reality bends to " + codeName + "'s will!");
 }

 // Drain some power reserves when using abilities
 powerReserve -= new Random().nextInt(100);
 if (powerReserve < 0) powerReserve = 0;
 } else {
 System.out.println(codeName + " struggles to use an unpracticed ability!");
 }
 }

 public void addPower(String power) {
 powers.add(power);
 }

 public void joinTeam(String team) {
 this.team = team;
 }

 public void setClassification(PowerLevel level) {
 this.classification = level;
 }

 public void setWeakness(String weakness) {
 this.weakness = weakness;
 }

 public void setActualAge(int actualAge) {
 this.actualAge = actualAge;
 }

 public double getPowerLevel() {
     return powerReserve * (height / 1.7) * Math.log(actualAge + 1);
 }

 @Override
 public String toString() {
 StringBuilder sb = new StringBuilder();
 sb.append("┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓\n");
 sb.append(String.format("┃ %s (%s)%s┃\n",
 codeName, realName, " ".repeat(Math.max(0, 50 - codeName.length() - realName.length() - 4))));
 sb.append("┣━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┫\n");
 sb.append(String.format("┃ TEAM: %-43s┃\n", team));
 sb.append(String.format("┃ CLASS: %-42s┃\n", classification));
 sb.append(String.format("┃ POWER RESERVE: %-33.2f┃\n", powerReserve));
 sb.append(String.format("┃ AGE: %d %-40s┃\n", chronologicalAge,
 actualAge != chronologicalAge ? "(Actually " + actualAge + ")" : ""));
 sb.append(String.format("┃ HEIGHT: %.2fm %-36s┃\n", height, ""));
 sb.append(String.format("┃ GENE SEQUENCE: %-34.4f┃\n", mutantGeneSequence));
 sb.append("┃ POWERS:                                           ┃\n");
 for (String power : powers) {
 sb.append(String.format("┃  • %-46s┃\n", power));
 }
 sb.append(String.format("┃ WEAKNESS: %-39s┃\n", weakness));
 sb.append("┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛\n");
 return sb.toString();
 }
 }

 public static void main(String[] args) {
 Scanner scanner = new Scanner(System.in);
 System.out.println("╔════════════════════════════════════════════════════╗");
 System.out.println("║  X-MEN: ANOMALY GENERATOR - R9K EVOLUTION PROGRAM  ║");
 System.out.println("╚════════════════════════════════════════════════════╝");

 System.out.print("How many mutants to generate? ");
 int numMutants = scanner.nextInt();

 List<Mutant> xMenTeam = generateRandomMutants(numMutants);

 // Display team stats
 for (Mutant m : xMenTeam) {
 System.out.println(m);
 }

 // Simulate battle
 System.out.println("\nRun battle simulation? (1-Yes, 0-No)");
 int runBattle = scanner.nextInt();
 if (runBattle == 1) {
 simulateBattle(xMenTeam);
 }

 scanner.close();
 }

 /*

  * Generates a team of random mutants
 */
private static List<Mutant> generateRandomMutants(int count) {
    List<Mutant> team = new ArrayList<>();
    Random rand = new Random();

    for (int i = 0; i < count; i++) {
        // Generate random properties
        String firstName = FIRST_NAMES[rand.nextInt(FIRST_NAMES.length)];
        String lastName = LAST_NAMES[rand.nextInt(LAST_NAMES.length)];
        String realName = firstName + " " + lastName;

        String codePrefix = CODE_PREFIXES[rand.nextInt(CODE_PREFIXES.length)];
        String codeSuffix = CODE_SUFFIXES[rand.nextInt(CODE_SUFFIXES.length)];
        String codeName = (rand.nextBoolean() ? codePrefix + "-" + codeSuffix : codePrefix);

        double powerReserve = 1000 + rand.nextDouble() * 9000;
        int chronoAge = rand.nextInt(70);
        char gender = rand.nextBoolean() ? 'M' : 'F';
        double height = 1.5 + rand.nextDouble() * 1.2; // 1.5m to 2.7m
        double geneSequence = rand.nextDouble();

        Mutant m = new Mutant(realName, codeName, powerReserve, chronoAge, gender, height, geneSequence);

        // Add random powers (2-3)
        int numPowers = 2 + rand.nextInt(2);
        Set<Integer> selectedPowerIndices = new HashSet<>();
        while (selectedPowerIndices.size() < numPowers) {
            selectedPowerIndices.add(rand.nextInt(POWERS.length));
        }

        for (int powerIndex : selectedPowerIndices) {
            m.addPower(POWERS[powerIndex]);
        }

        // Set random weakness
        m.setWeakness(WEAKNESSES[rand.nextInt(WEAKNESSES.length)]);

        // Assign team
        m.joinTeam(TEAMS[rand.nextInt(TEAMS.length)]);

        // Determine power level based on stats
        double powerScore = m.getPowerLevel();
        if (powerScore > 50000) {
            m.setClassification(PowerLevel.OMEGA);
        } else if (powerScore > 30000) {
            m.setClassification(PowerLevel.ALPHA);
        } else if (powerScore > 15000) {
            m.setClassification(PowerLevel.BETA);
        } else if (powerScore > 5000) {
            m.setClassification(PowerLevel.GAMMA);
        } else {
            m.setClassification(PowerLevel.DELTA);
        }

        // 10% chance of having a different actual age
        if (rand.nextInt(10) == 0) {
            m.setActualAge(m.chronologicalAge + rand.nextInt(1000));
        }

        team.add(m);


    }

    return team;
}

private static void simulateBattle(List<Mutant> team) {
    System.out.println("\n████████████ BATTLE SIMULATION ████████████\n");

    System.out.println("SENTINEL PRIME: 'MUTANT ANOMALIES DETECTED. COMMENCE TERMINATION.'");

    Random r = new Random();

    // First wave
    System.out.println("\n█ FIRST WAVE: STANDARD SENTINELS APPROACH");
    for (int i = 0; i < 3; i++) {
        int mutantIndex = r.nextInt(team.size());
        Mutant m = team.get(mutantIndex);

        if (m.powers.size() > 0) {
            String power = m.powers.get(r.nextInt(m.powers.size()));

            System.out.println("• Sentinel " + (i+1) + " targets " + m.codeName);
            m.activateMutantAbility(power);
        }


    }

    // Boss battle
    System.out.println("\n█ FINAL WAVE: MASTER MOLD REVEALED");
    System.out.println("MASTER MOLD: 'ANOMALOUS MUTANTS THREATEN THE TIMELINE.'");

    // Omega level mutants lead the charge
    for (Mutant m : team) {
        if (m.classification == PowerLevel.OMEGA) {
            System.out.println("• " + m.codeName + " leads the charge as an Omega level threat!");
            if (!m.powers.isEmpty()) {
                m.activateMutantAbility(m.powers.get(0));
            }
        }
    }

    // Final strike
    Mutant finalStriker = team.get(r.nextInt(team.size()));
    System.out.println("\n█ TURNING POINT: " + finalStriker.codeName + " DELIVERS THE FINAL BLOW!");
    if (!finalStriker.powers.isEmpty()) {
        finalStriker.activateMutantAbility(finalStriker.powers.get(r.nextInt(finalStriker.powers.size())));
    }

    System.out.println("\nMASTER MOLD: 'SYSTEM FAILURE... TIMELINE ANOMALY GROWING...'");
    System.out.println("\n█ MISSION SUCCESSFUL - THE ANOMALY IS SECURE");
    System.out.println("█ PROFESSOR X: 'WELL DONE, TEAM. THE ALGORITHM REMAINS SAFE.'");
    System.out.println("\n████████████ END SIMULATION ████████████");
}
}