trait Trait {
    // In Groovy traits, just declare methods without abstract keyword
    void print(){
        println "Print method from trait"
    }

    // Default implementation
    void printDefault() {
        println "Default method"
    }
}

class ConcreteT implements Trait {
    void print() {
        println "Print method from concrete implementation"
    }
}

static void main(String[] args) {
    println "Hello world!"
    def x = 10
    println x
    def y = "Number: $x"
    println y

    // intdiv and mod
    println 10.intdiv(3)
    println 10.mod(3)

    // range
    def range = 1..10
    println range
    for (i in range) {
        println i
    }

    // step 2
    for (i in range.step(2)) {
        println i
    }

    // step 3
    for (i in range.step(3)) {
        println i
    }

    //limits
    def biggestInt = Integer.MAX_VALUE
    println biggestInt
    def smallestInt = Integer.MIN_VALUE
    println smallestInt
    def biggestLong = Long.MAX_VALUE
    println biggestLong
    def smallestLong = Long.MIN_VALUE
    println smallestLong
    def biggestFloat = Float.MAX_VALUE
    println biggestFloat
    def smallestFloat = Float.MIN_VALUE
    println smallestFloat
    def biggestDouble = Double.MAX_VALUE
    println biggestDouble
    def smallestDouble = Double.MIN_VALUE
    println smallestDouble

    // Trait usage - need concrete implementation
    Trait t = new ConcreteT()
    t.print()
    t.printDefault()
    // math methods
    println Math.abs(-10)
    println Math.max(10, 20)
    println Math.min(10, 20)
    println Math.pow(2, 3)
    println Math.sqrt(9)
    println Math.random()
    println Math.round(3.14)
    println Math.ceil(3.14)
    println Math.floor(3.14)
    println Math.nextUp(3.14)
    println Math.nextDown(3.14)
    println Math.toDegrees(Math.PI)
    println Math.toRadians(180)
    println Math.sin(Math.PI / 2)
    println Math.cos(Math.PI / 2)
    println Math.tan(Math.PI / 2)
    println Math.asin(1)
    println Math.acos(0)
    println Math.atan(0)
    println Math.toDegrees(Math.atan(1))
    println Math.tanh(3)
    println Math.sinh(3)
    println Math.cosh(3)

    // Proper 2D array handling
    def mat = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

    // Create a proper 10x10 matrix instead of this cursed expansion
    def bigMat = new int[10][10]
    for(i in 0..9) {
        for(j in 0..9) {
            bigMat[i][j] = (i + 1) * (j + 1)  // +1 for 1-based math
        }
    }

    // Print original matrix
    for (row in mat) {
        for (val in row) {
            print "$val "
        }
        println()
    }

    // maps
    def map = [1: "one", 2: "two", 3: "three"]
    println map
    map[4] = "four"
    println map
    map.put(5, "five")
    println map
    map.remove(5)
    println map

    map.each { k, v ->
        println "$k: $v"
    }

    map.eachWithIndex { entry, i ->
        println "$i: ${entry.key} -> ${entry.value}"
    }

    // sets (use Set, not List)
    def set = [1, 2, 3, 4, 5] as Set
    println set
    set << 6
    println set
    set.add(7)
    println set
    set.remove(7)
    println set

    // lists (what you called arrays/tuples)
    def list = [1, 2, 3]
    println list
    println list[0]
    println list[1]
    println list[2]
    println list[0..1]
    closure()
    // Single random closure
    def random = { min, max -> new Random().nextInt(max - min + 1) + min }
    println "Random: ${random(1, 10)}"

    // Proper recursive fibonacci
    def fibonacci
    fibonacci = { n ->
        if (n <= 2) {
            return 1
        } else {
            return fibonacci(n - 1) + fibonacci(n - 2)
        }
    }
    println "Fibonacci(10): ${fibonacci(10)}"

    // Proper factorial function
    def factorial
    factorial = { n ->
        if (n == 0) {
            return 1
        } else {
            return n * factorial(n - 1)
        }
    }
    println "Factorial(5): ${factorial(5)}"

    // Simple greeting function
    def hello = { name ->
        println "Hello, $name!"
    }
    hello("World")

    // Basic linear regression (simplified)
    def linearRegression = { xValues, yValues ->
        def n = xValues.size()
        def sumX = xValues.sum()
        def sumY = yValues.sum()
        def sumXY = [xValues, yValues].transpose().collect { it[0] * it[1] }.sum()
        def sumXX = xValues.collect { it * it }.sum()

        def slope = (n * sumXY - sumX * sumY) / (n * sumXX - sumX * sumX)
        def intercept = (sumY - slope * sumX) / n

        return [intercept, slope]
    }

    def xData = [1, 2, 3, 4, 5]
    def yData = [2, 4, 6, 8, 10]
    def result = linearRegression(xData, yData)
    println "Linear regression: y = ${result[1]}x + ${result[0]}"

    println "Splitting \$1.17:"
    split(1.17)

    "Groovy".split("") groupBy{it} collectEntries{ k,v -> [k, v.size()] }
}
void closure() {
    def add = { x, y -> x + y }
    println add(1, 2)
    def multiply = { x, y -> x * y }
    println multiply(2, 3)
    def subtract = { x, y -> x - y }
    println subtract(5, 2)
    def divide = { x, y -> x / y }
    println divide(10, 2)
    def power = { x, y -> x**y }
    println power(2, 3)
    def mod = { x, y -> x % y }
    println mod(10, 3)
}

def split(float amount){
    int rest = (amount * 100).round() as int // <- Groovy is shit at this
    String[] pretty_names = ["Quarters", "Dimes", "Nickles", "Pennies"]
    [25, 10, 5, 1].eachWithIndex { coin_size, index ->
        println "${rest/coin_size as int} ${pretty_names[index]}"
        rest = rest % coin_size
    }
}
