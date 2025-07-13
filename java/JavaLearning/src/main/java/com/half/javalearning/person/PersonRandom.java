package com.half.javalearning.person;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

class Perks {
    private int level;
    private int xp;
    private int health;
    private int stamina;
    private int mana;

    // Core RPG stats
    private int strength;
    private int dexterity;
    private int intelligence;
    private int charisma;
    private int wisdom;
    private int perception;
    private int luck;

    // Additional traits
    private String weaponProficiency;
    private String spellMastery;
    private String faction;
    private String curse;
    private String blessing;
    private String alignment;
    private String combatStyle;
    private String resistance;
    private String weakness;
    private String title;
    private String reputation;
    private String[] inventory;

    private Random random;

    public Perks() {
        random = new Random();
        this.level = random.nextInt(100) + 1;
        this.xp = random.nextInt(10000);
        this.health = random.nextInt(500) + 100; // Health between 100 and 600
        this.stamina = random.nextInt(300) + 50; // Stamina between 50 and 350
        this.mana = random.nextInt(300) + 50; // Mana between 50 and 350

        // Core stats
        this.strength = random.nextInt(20) + 1; // Strength between 1 and 20
        this.dexterity = random.nextInt(20) + 1; // Dexterity between 1 and 20
        this.intelligence = random.nextInt(20) + 1; // Intelligence between 1 and 20
        this.charisma = random.nextInt(20) + 1; // Charisma between 1 and 20
        this.wisdom = random.nextInt(20) + 1; // Wisdom between 1 and 20
        this.perception = random.nextInt(20) + 1; // Perception between 1 and 20
        this.luck = random.nextInt(20) + 1; // Luck between 1 and 20

        // Additional traits
        this.weaponProficiency = getRandomWeapon();
        this.spellMastery = getRandomSpell();
        this.faction = getRandomFaction();
        this.curse = getRandomCurse();
        this.blessing = getRandomBlessing();
        this.alignment = getRandomAlignment();
        this.combatStyle = getRandomCombatStyle();
        this.resistance = getRandomResistance();
        this.weakness = getRandomWeakness();
        this.title = getRandomTitle();
        this.reputation = getRandomReputation();
        this.inventory = generateRandomInventory(5);
    }

    private String getRandomWeapon() {
        String[] weapons = {"Sword", "Bow", "Axe", "Dagger", "Staff", "Mace", "Greatsword", "Halberd", "Crossbow"};
        return weapons[random.nextInt(weapons.length)];
    }

    private String getRandomSpell() {
        String[] spells = {"Fireball", "Frostbite", "Lightning Bolt", "Soul Trap", "Healing", "Invisibility", "Summon Daedra", "Necromancy"};
        return spells[random.nextInt(spells.length)];
    }

    private String getRandomFaction() {
        String[] factions = {"Dark Brotherhood", "Thieves Guild", "Companions", "College of Winterhold", "Blades", "Stormcloaks", "Imperial Legion", "Forsworn"};
        return factions[random.nextInt(factions.length)];
    }

    private String getRandomCurse() {
        String[] curses = {"Hollowing", "Curse of Weakness", "Mark of the Beast", "Soul Drain", "Eternal Hunger", "Nightmare Plague", "Bloodlust"};
        return curses[random.nextInt(curses.length)];
    }

    private String getRandomBlessing() {
        String[] blessings = {"Blessing of Talos", "Divine Protection", "Aura of Strength", "Gift of Magicka", "Shadow's Grace", "Light of the Nine", "Dragon's Favor"};
        return blessings[random.nextInt(blessings.length)];
    }

    private String getRandomAlignment() {
        String[] alignments = {"Lawful Good", "Neutral Good", "Chaotic Good", "Lawful Neutral", "True Neutral", "Chaotic Neutral", "Lawful Evil", "Neutral Evil", "Chaotic Evil"};
        return alignments[random.nextInt(alignments.length)];
    }

    private String getRandomCombatStyle() {
        String[] styles = {"Aggressive", "Defensive", "Balanced", "Stealthy", "Magic-Focused", "Berserker", "Sniper"};
        return styles[random.nextInt(styles.length)];
    }

    private String getRandomResistance() {
        String[] resistances = {"Fire", "Frost", "Shock", "Poison", "Disease", "Magic", "Physical"};
        return resistances[random.nextInt(resistances.length)];
    }

    private String getRandomWeakness() {
        String[] weaknesses = {"Fire", "Frost", "Shock", "Poison", "Disease", "Magic", "Physical"};
        return weaknesses[random.nextInt(weaknesses.length)];
    }

    private String getRandomTitle() {
        String[] titles = {"Dragonborn", "Champion of Cyrodiil", "Archmage", "Thane of Whiterun", "Shadow of the Night", "Warden of the North", "Slayer of Beasts"};
        return titles[random.nextInt(titles.length)];
    }

    private String getRandomReputation() {
        String[] reputations = {"Heroic", "Infamous", "Mysterious", "Respected", "Feared", "Renowned", "Disgraced"};
        return reputations[random.nextInt(reputations.length)];
    }

    private String[] generateRandomInventory(int size) {
        String[] items = {"Health Potion", "Mana Potion", "Lockpick", "Soul Gem", "Dragonbone", "Gold Coin", "Torch", "Scroll of Fireball", "Elven Arrow"};
        String[] inventory = new String[size];
        for (int i = 0; i < size; i++) {
            inventory[i] = items[random.nextInt(items.length)];
        }
        return inventory;
    }

    public void displayPerks() {
        System.out.println("=== Perks ===");
        System.out.println("Level: " + level);
        System.out.println("XP: " + xp);
        System.out.println("Health: " + health);
        System.out.println("Stamina: " + stamina);
        System.out.println("Mana: " + mana);
        System.out.println("Strength: " + strength);
        System.out.println("Dexterity: " + dexterity);
        System.out.println("Intelligence: " + intelligence);
        System.out.println("Charisma: " + charisma);
        System.out.println("Wisdom: " + wisdom);
        System.out.println("Perception: " + perception);
        System.out.println("Luck: " + luck);
        System.out.println("Weapon Proficiency: " + weaponProficiency);
        System.out.println("Spell Mastery: " + spellMastery);
        System.out.println("Faction: " + faction);
        System.out.println("Curse: " + curse);
        System.out.println("Blessing: " + blessing);
        System.out.println("Alignment: " + alignment);
        System.out.println("Combat Style: " + combatStyle);
        System.out.println("Resistance: " + resistance);
        System.out.println("Weakness: " + weakness);
        System.out.println("Title: " + title);
        System.out.println("Reputation: " + reputation);
        System.out.println("Inventory: " + String.join(", ", inventory));
    }
}
class Person {
    private String name;
    private int age;
    private Perks perks;
    private String[] hobbies;
    private String[] interests;

    private Random random;

    public Person() {
        random = new Random();
        this.name = generateRandomFullName(3, 10, random);
        this.age = random.nextInt(100) + 1;
        this.perks = new Perks();
        this.hobbies = generateRandomArray("Hobby", 3);
        this.interests = generateRandomArray("Interest", 3);
    }

    public static String generateRandomName(int minLength, int maxLength, Random random) {
        int length = random.nextInt(maxLength - minLength + 1) + minLength;
        StringBuilder sb = new StringBuilder();
        boolean isConsonant = true;
        for (int i = 0; i < length; i++) {
            char randomChar = isConsonant ? Person.getRandomConsonant(random) : Person.getRandomVowel(random);
            sb.append(randomChar);
            isConsonant = !isConsonant;
        }
        sb.setCharAt(0, Character.toUpperCase(sb.charAt(0)));
        return sb.toString();
    }
    public static String generateRandomFullName(int minLength, int maxLength, Random random) {
        return Person.generateRandomName(minLength, maxLength, random) + " " + Person.generateRandomName(minLength, maxLength, random);
    }

    private static char getRandomConsonant(Random random) {
        String consonants = "bcdfghjklmnpqrstvwxyz";
        return consonants.charAt(random.nextInt(consonants.length()));
    }

    private static char getRandomVowel(Random random) {
        String vowels = "aeiou";
        return vowels.charAt(random.nextInt(vowels.length()));
    }

    public String[] generateRandomArray(String prefix, int size) {
        String[] array = new String[size];
        for (int i = 0; i < size; i++) {
            array[i] = prefix + (i + 1);
        }
        return array;
    }

    public void displayPersonDetails() {
        System.out.println("Name: " + name);
        System.out.println("Age: " + age);
        System.out.println("Hobbies: " + String.join(", ", hobbies));
        System.out.println("Interests: " + String.join(", ", interests));
        System.out.println("Perks:");
        perks.displayPerks();
    }

    public String getName() {
        return name;
    }
}

class Team {
    private String name;
    private List<AlunoRandom> members;

    public Team(String name, int memberCount) {
        this.name = name;
        this.members = new ArrayList<>();
        for (int i = 0; i < memberCount; i++) {
            members.add(new AlunoRandom(i + 1));
        }
    }

    public void displayTeamDetails() {
        System.out.println("Team Name: " + name);
        System.out.println("Members:");
        for (AlunoRandom member : members) {
            member.displayAlunoDetails();
            System.out.println();
        }
    }
}

class AlunoRandom {
    private String name;
    private int age;
    private int id;
    private List<String> subjects;
    private List<Integer> scores;

    private Random random;

    public AlunoRandom(int id) {
        random = new Random();
        this.id = id;
        this.name = getRandomName();
        this.age = random.nextInt(10) + 15; // Age between 15 and 25
        this.subjects = generateRandomSubjects();
        this.scores = generateRandomScores(subjects.size());
    }

    private String getRandomName() {
        String[] names = {"Alice", "Bob", "Charlie", "Diana", "Ethan", "Fiona", "George", "Hannah", "Ivan", "Julia"};
        return names[random.nextInt(names.length)];
    }

    private List<String> generateRandomSubjects() {
        String[] allSubjects = {"Math", "Science", "History", "English", "Art", "Music", "Physical Education", "Biology", "Chemistry"};
        int subjectCount = random.nextInt(4) + 2; // Each student studies 2-5 subjects
        List<String> selectedSubjects = new ArrayList<>();
        for (int i = 0; i < subjectCount; i++) {
            String subject;
            do {
                subject = allSubjects[random.nextInt(allSubjects.length)];
            } while (selectedSubjects.contains(subject));
            selectedSubjects.add(subject);
        }
        return selectedSubjects;
    }

    private List<Integer> generateRandomScores(int size) {
        List<Integer> scores = new ArrayList<>();
        for (int i = 0; i < size; i++) {
            scores.add(random.nextInt(101)); // Scores between 0 and 100
        }
        return scores;
    }

    public void displayAlunoDetails() {
        System.out.println("Aluno ID: " + id);
        System.out.println("Name: " + name);
        System.out.println("Age: " + age);
        System.out.println("Subjects and Scores:");
        for (int i = 0; i < subjects.size(); i++) {
            System.out.println("  " + subjects.get(i) + ": " + scores.get(i));
        }
    }
}

class Grupo {
    private String groupName;
    private String professor;
    private String projectTopic;
    private String meetingSchedule;
    private List<AlunoRandom> alunos;

    private Random random;

    public Grupo(int alunoCount) {
        random = new Random();
        this.groupName = getRandomGroupName();
        this.professor = getRandomProfessor();
        this.projectTopic = getRandomProjectTopic();
        this.meetingSchedule = getRandomMeetingSchedule();
        this.alunos = generateAlunos(alunoCount);
    }

    private String getRandomGroupName() {
        String[] groupNames = {"Grupo Alpha", "Grupo Beta", "Grupo Gamma", "Grupo Delta", "Grupo Omega"};
        return groupNames[random.nextInt(groupNames.length)];
    }

    private String getRandomProfessor() {
        String[] professors = {"Dr. John Smith", "Prof. Emily Carter", "Dr. Michael Brown", "Prof. Sarah Johnson", "Dr. William Davis"};
        return professors[random.nextInt(professors.length)];
    }

    private String getRandomProjectTopic() {
        String[] topics = {"Renewable Energy Research", "Artificial Intelligence", "Climate Change Impact", "Space Exploration", "Genetic Engineering"};
        return topics[random.nextInt(topics.length)];
    }

    private String getRandomMeetingSchedule() {
        String[] schedules = {"Every Monday at 10 AM", "Every Wednesday at 2 PM", "Every Friday at 3 PM", "Every Saturday at 1 PM", "Every Thursday at 4 PM"};
        return schedules[random.nextInt(schedules.length)];
    }

    private List<AlunoRandom> generateAlunos(int count) {
        List<AlunoRandom> alunos = new ArrayList<>();
        for (int i = 0; i < count; i++) {
            alunos.add(new AlunoRandom(i + 1));
        }
        return alunos;
    }

    public void displayGrupoDetails() {
        System.out.println("=== Grupo Details ===");
        System.out.println("Group Name: " + groupName);
        System.out.println("Professor: " + professor);
        System.out.println("Project Topic: " + projectTopic);
        System.out.println("Meeting Schedule: " + meetingSchedule);
        System.out.println("Alunos:");
        for (AlunoRandom aluno : alunos) {
            aluno.displayAlunoDetails();
            System.out.println();
        }
    }
}
class PersonRandom {
    public static void main(String[] args) {
        Team team = new Team("Team Alpha", 3);

        System.out.println("=== Team Details ===");
        //team.displayTeamDetails();

        System.out.println("\n=== Group Details ===");
            Grupo grupo = new Grupo(7); // Create a group with 5 students
            grupo.displayGrupoDetails();

        for(int i = 0; i < 3; i++) {
        Person person = new Person();
        person.displayPersonDetails();
        }
    }
}