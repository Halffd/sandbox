import kotlin.random.Random

fun generateRandomName(length: Int): String {
    val allowedChars = ('A'..'Z') + ('a'..'z')
    return (1..length)
        .map { allowedChars.random() }
        .joinToString("")
}

fun pingGoogle() {
    try {
        val process = ProcessBuilder("ping", "-c", "4", "8.8.8.8") // Added timeout
            .redirectOutput(ProcessBuilder.Redirect.INHERIT)
            .redirectError(ProcessBuilder.Redirect.INHERIT)
            .start()

        if (process.waitFor() != 0) {
            throw Exception("Ping failed")
        }
    } catch (e: Exception) {
        println("Ping failed: ${e.message}")
        throw e
    }
}
fun lists(): List<Any> {
    val buyList = listOf(
        "apple", 10,
        "banana", 5,
        "cherry", 3,
        true,
        "date", 2,
        "elderberry", 1.toString(),
        listOf(1, 2, 3, 4, 5),
        listOf(
            "apple", 10,
            "banana", 5,
            "cherry", 3,
            true,
            "date", 2,
            "elderberry", 1.toString(),
            arrayListOf<Any?>(1, "", null)
        )
    )
    buyList.forEach { println(it) }
    println(buyList.toString())
    val prices = listOf(buyList.size, 5, 3, 2, 1)
    println(prices.toString())
    println(prices.sum())
    val normalMap = mapOf("Europe" to "Paris", "North America" to "Washington", "South America" to "Santiago", "Asia" to "Tokyo", "Africa" to "Cairo", "Oceania" to "Sydney", "Antarctica" to "McMurdo")
    println(normalMap["Europe"])
    println(normalMap["North America"])
    println(normalMap["South America"])
    println(normalMap["Asia"])
    println(normalMap["Africa"])
    println(normalMap["Oceania"])
    println(normalMap.toString())
    for(location in normalMap.keys) {
        when(location) {
            "Europe" -> println("Europe is a continent")
            "North America" -> println("North America is a continent")
            "South America" -> println("South America is a continent")
            "Asia" -> println("Asia is a continent")
            "Africa" -> println("Africa is a continent")
            "Oceania" -> println("Oceania is a continent")
            else -> println("Unknown continent")
        }
        println("$location: ${normalMap[location]}")
    }
    val buyMap = mutableMapOf("apple" to 10, "banana" to 5, "cherry" to 3, "date" to 2, "elderberry" to 1, buyList.get(0) to buyList.get(1), buyList.get(2) to buyList.get(3), buyList.get(4) to buyList.get(5), buyList.get(6) to buyList.get(7), buyList.get(8) to buyList.get(9))
    println(buyMap["apple"])
    println(buyMap.toString())
    for((key, value) in buyMap) { 
        if(value is Int) {
            buyMap[key] = value + 1
        }
        val stringValue = buyMap[key].toString()
        println("$key: $stringValue")
    }
    //merge all of them together
    val mergedMap = buyMap + normalMap
    println(mergedMap.toString())
    mergedMap.values.filterIsInstance<String>()
        .map { it.uppercase() }
        .sortedDescending()
        .forEach { println(it) }
    val mergedList = buyList + mergedMap
    mergedList.forEach {  thing -> 
        //reversed string
        if (thing is String) {
            println(thing.reversed())
        }
    }
    println(mergedList.toString())
    //all to uppercase
    val upperCaseList = mergedList.map { if (it is String) it.uppercase() else it }
    println(upperCaseList.toString())
    //all to lowercase
    val lowerCaseList = mergedList.map { if (it is String) it.lowercase() else it }
    println(lowerCaseList.toString())
    //first leter to uppercase
    val firstLetterUpperCaseList = mergedList.map { if (it is String) it.replaceFirstChar { it.uppercase() } else it }
    println(firstLetterUpperCaseList.toString())
    return firstLetterUpperCaseList
}
fun main() {
    var name = lists()[0] // Not nullable, you muppet
    Greeter(name as String).greet()
    fun singleLineAssignment(name: String): String = name
    singleLineAssignment("Kotlin")
    println("Hello, $name!")

    for (i in 1..5) {
        println("i = $i")
    }

    try {
        pingGoogle()
    } catch (e: Exception) {
        println("Network check failed, continuing anyway...")
    }

    // Fixed your schizophrenic string manipulation
    while (name.notEmpty()) {
        println("Current name: $name")

        // Remove first character
        name = (name as String).drop(1)

        if (name.notEmpty()) {
            // Adjust length if odd
            if (name.length % 2 != 0) {
                name = name.dropLast(1)
            }

            // Add random chars if divisible by 4
            if (name.length % 4 == 0 && name.isNotEmpty()) {
                name += generateRandomName(name.length)
                println("Added random chars: $name")
            }
        }
    }

    println("Final name: '$name'")
}
private fun Any?.notEmpty(): Boolean = try {
    when (this) {
        null -> false
        is String -> this.isNotEmpty() // Use String's built-in method
        else -> this.toString().isNotEmpty() // This will work now
    }
} catch (e: Exception) {
    false
}