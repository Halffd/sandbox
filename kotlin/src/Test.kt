class Test {
    fun test() {
        println("test")
    }

    fun unitTest() {
        println("unit test")
    }

    fun stressTest() {
        // Fixed: removed the recursive suicide call
        for (i in 1..100) { // Also reduced from 10k because I'm not Satan
            Thread { test() }.start()
            Thread { unitTest() }.start()
        }
        // Wait for threads to finish like a civilized human
        Thread.sleep(1000)
    }
}

open class Weeb {
    val 最初 = 0
    val 最後 = 100
    val 蝶 = "Fizz"
    val 蜂 = "Buzz"
    val 笑 = 蝶 + 蜂

    fun 笑(笑: Int) = 笑 % 15 == 0
    fun 喋(言葉: String) = println(言葉)
    fun 蝶(蝶: Int) = 蝶 % 3 == 0
    fun 蜂(蜂: Int) = 蜂 % 5 == 0
    fun <T> Iterable<T>.数える(action: (T) -> Unit) = this.forEach(action)
    fun Int.喋() = toString()

    fun wwwww(ws: Int) {
        repeat(ws) { print("w") }
        println()
    }

    fun runWeebFizz() {
        (最初..最後).数える {
            when {
                笑(it) -> 喋(笑)
                蝶(it) -> 喋(蝶)
                蜂(it) -> 喋(蜂)
                else -> 喋(it.喋())
            }
        }
    }

    open fun main() {
        println("=== Weeb FizzBuzz ===")
        runWeebFizz()
        println("\n=== WWW Test ===")
        wwwww(3)
        wwwww(5)
        wwwww(15)
        println("\n=== Stress Test ===")
        Test().stressTest()
    }
}

class UltraWeeb : Weeb() {
    // Different variable names to avoid confusion
    private val フィズ = "Fizz"
    private val バズ = "Buzz"
    private val フィズバ = フィズ + バズ

    fun ファックユー(イルカ: Int) = イルカ % 15 == 0
    fun イズフィズ(泡: Int) = 泡 % 3 == 0
    fun イズバズ(爆: Int) = 爆 % 5 == 0
    fun 喋って(言葉: String) = println(言葉)
    fun <T> Iterable<T>.並べ立て(行動: (T) -> Unit) = this.forEach(行動)
    fun Int.言葉() = toString()

    fun ayaya(aya: Int) {
        repeat(aya) { print("AYAYA ") }
        println()
    }

    fun runUltraWeebFizz() {
        (最初..最後).並べ立て { 物 ->
            when {
                ファックユー(物) -> 喋って(フィズバ)
                イズフィズ(物) -> 喋って(フィズ)
                イズバズ(物) -> 喋って(バズ)
                else -> 喋って(物.言葉())
            }
        }
    }

    override fun main() {
        super.main()
        println("\n=== ULTRA WEEB MODE ACTIVATED ===")
        runUltraWeebFizz()
        println("\n=== AYAYA SPAM ===")
        ayaya(3)
        ayaya(5)
        ayaya(15)
        ayaya(30)
    }
}

fun main() {
    UltraWeeb().main()
}