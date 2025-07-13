trait Toddler {
    // Toddler behaviors
    void cry() {
        println "WAAAHHHHH! *throws tantrum*"
    }

    void babble() {
        def words = ["mama", "dada", "no", "mine", "cookie"]
        println words[new Random().nextInt(words.size())]
    }

    void makeNoise() {
        def noises = ["*screaming*", "*giggling*", "*babbling*", "*crying*", "*laughing maniacally*"]
        println noises[new Random().nextInt(noises.size())]
    }

    // Default toddler chaos level
    int getChaosLevel() {
        return new Random().nextInt(10) + 1
    }

    void causeHavoc() {
        println "Toddler throws food everywhere and demands snacks 5 minutes later"
    }
}

class Baby implements Toddler {
    String name
    int age
    boolean isAwake = true
    boolean isHungry = true
    boolean needsDiaper = true

    Baby(String name, int age) {
        this.name = name
        this.age = age
    }

    // Baby-specific methods
    void sleep() {
        isAwake = false
        println "$name is finally asleep... for 20 minutes"
    }

    void wakeUp() {
        isAwake = true
        cry()
        println "$name is awake and ready to destroy your sanity"
    }

    void eat() {
        if (isHungry) {
            isHungry = false
            needsDiaper = true
            println "$name ate food and immediately needs diaper change"
        } else {
            println "$name refuses food and throws it on the floor instead"
        }
    }

    void changeDiaper() {
        if (needsDiaper) {
            needsDiaper = false
            println "Diaper changed! $name immediately poops again"
            needsDiaper = true
        } else {
            println "$name doesn't need diaper change but cries anyway"
        }
    }

    // Override trait method for baby-specific crying
    @Override
    void cry() {
        def cryTypes = ["*soft whimper*", "WAAAHHHHH!", "*angry screaming*", "*pathetic sobbing*"]
        println "$name: ${cryTypes[new Random().nextInt(cryTypes.size())]}"
    }

    void growUp() {
        age++
        if (age >= 2) {
            println "$name is now a proper toddler - chaos level increased to MAXIMUM"
        }
    }

    @Override
    String toString() {
        return "$name (age: $age, awake: $isAwake, hungry: $isHungry, needs diaper: $needsDiaper)"
    }
}

static void main(String[] args) {
    // Create some babies
    def baby1 = new Baby("Little Chaos", 1)
    def baby2 = new Baby("Tiny Destroyer", 2)

    println baby1
    println baby2

    // Baby behaviors
    baby1.cry()
    baby1.babble()
    baby1.makeNoise()
    baby1.eat()
    baby1.changeDiaper()
    baby1.sleep()
    baby1.wakeUp()

    println "Chaos level: ${baby1.getChaosLevel()}/10"
    baby1.causeHavoc()

    // Age progression
    baby1.growUp()
    baby1.growUp() // Now a toddler

    println "\nAfter growing up:"
    println baby1

    // Toddler chaos
    baby2.cry()
    baby2.causeHavoc()
    println "Toddler chaos level: ${baby2.getChaosLevel()}/10"

    // The eternal cycle
    for (int i = 0; i < 5; i++) {
        baby1.makeNoise()
        Thread.sleep(100) // Brief pause between chaos
    }

    println "\nParent's sanity level: 0/10"
}