
class Greeter(val name: String) {
    fun greet() {
        println("Hello, $name")
    }
}
class BestRegarder(val name: String) {
    fun sendBestRegards() {
        println("Best Regards, $name")
    }
}
data class Person(val name: String) {
    fun greet() = println("Hello, $name")
    fun sendRegards() = println("Best Regards, $name")
}

fun main(args: Array<String>) {
    Greeter("World").greet()
    
    BestRegarder("Kotlin").sendBestRegards()
    val person = Person("Kotlin")
    person.greet()
    person.sendRegards()
    if (args.isEmpty()) {
        println("Please provide a name as a command-line argument")
        return
    }
    println("Hello, ${args[0]}!")
}