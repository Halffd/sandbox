import scala.util.{Try, Success, Failure}
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.math._
import scala.util.Random
import scala.util.control.Exception._

@main
def main(): Unit =
  // Basic math examples
  println("=== Basic Math ===")
  val largePrime = BigInt("170141183460469231731687303715884105727") // A known large prime (2^127 - 1)
  println(s"A large prime number: $largePrime")
  
  val pi = BigDecimal("3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679")
  println(s"Pi to 100 digits: $pi")
  
  // Function examples
  println("\n=== Functions ===")
  def minmax(a: Int, b: Int): (Int, Int) =
    if a < b then (a, b) else (b, a)
    
  val (min, max) = minmax(3, 5)
  println(s"min = $min, max = $max")
  
  // Guessing game
  def playGuessingGame(): Unit =
    val secret = (random * 67 + 33).toInt // Random number between 33 and 100
    println("\n=== Guessing Game ===")
    println("I'm thinking of a number between 33 and 100. Can you guess it?")
    
    @annotation.tailrec
    def guessLoop(): Unit =
      Try(scala.io.StdIn.readInt()) match {
        case Success(guess) =>
          if guess < secret then 
            println("Too low! Try again:")
            guessLoop()
          else if guess > secret then 
            println("Too high! Try again:")
            guessLoop()
          else 
            println("You guessed it!")
            
        case Failure(_) =>
          println("Please enter a valid number:")
          guessLoop()
      }
    
    guessLoop()
  
  playGuessingGame()
  
  // Compass direction to angle
  println("\n=== Compass Directions ===")
  def directionToAngle(direction: String): Option[Double] = direction match
    case "North" => Some(0.0)
    case "South" => Some(180.0)
    case "East"  => Some(90.0)
    case "West"  => Some(270.0)
    case _       => None
  
  val directions = List("North", "South", "East", "West", "Northwest")
  directions.foreach { dir =>
    directionToAngle(dir) match {
      case Some(angle) => 
        val rad = toRadians(angle)
        println(f"$dir%-8s = $angle%.1f° = $rad%.4f radians")
      case None => 
        println(s"Unknown direction: $dir")
    }
  }
  
  // Name generator example
  println("\n=== Name Generator ===")
  val name = NameGenerator.generateName()
  println(s"Generated name: $name")
  
  // Collection examples
  println("\n=== Collection Examples ===")
  val numbers = (1 to 5).toList
  val doubled = numbers.map(_ * 2)
  println(s"Numbers: $numbers")
  println(s"Doubled: $doubled")
  
  // Using Try for error handling
  println("\n=== Error Handling ===")
  def safeDivide(a: Int, b: Int) = Try(a / b)
  
  safeDivide(10, 2).foreach(result => println(s"10 / 2 = $result"))
  safeDivide(10, 0) match {
    case Success(result) => println(s"Result: $result")
    case Failure(e)      => println(s"Error: ${e.getMessage}")
  }
  
  // Using Future for asynchronous operations
  println("\n=== Asynchronous Operations ===")
  val futureResult = Future {
    Thread.sleep(1000) // Simulate work
    "Future completed successfully!"
  }
  
  futureResult.foreach(println)
  println("This prints before the future completes")
  Thread.sleep(1500) // Wait for future to complete
  
  // Ackermann function
  println("\n=== Ackermann Function ===")
  val ackermann = Ackermann.ackermann(3, 2)
  println(s"Ackermann(3, 2) = $ackermann")
  
  // Hotdog logic
  println("\n=== Hotdog Example ===")
  val isHotdog = "hotdog".startsWith("hot") && "hotdog".endsWith("dog")
  val food = "🌭"
  val isDotHotdog = if (food == "🌭") {
    println("hotdog")
    true
  } else {
    println("not hotdog")
    false
  }
  println(s"Is hotdog hot and dog? $isHotdog")
  println(s"Is food hotdog? $isDotHotdog")
  
  // Option-based division
  println("\n=== Option Example ===")
  def safeDivideOption(a: Int, b: Int): Option[Int] =
    if b == 0 then None else Some(a / b)

  safeDivideOption(10, 2).foreach(result => println(s"10 / 2 = $result"))
  safeDivideOption(10, 0) match {
    case Some(result) => println(s"Result: $result")
    case None         => println("Error: Division by zero")
  }

  val f = safeDivideOption(10, 2).flatMap(result => Some(result * 2))
  f match {
    case Some(result) => println(s"Result after flatMap: $result")
    case None         => println("Error: Division by zero")
  }

  // Either-based division
  println("\n=== Either Example ===")
  def safeDivideEither(a: Int, b: Int): Either[String, Int] =
    if b == 0 then Left("Division by zero") else Right(a / b)

  safeDivideEither(10, 2).foreach(result => println(s"10 / 2 = $result"))
  safeDivideEither(10, 0) match {
    case Right(result) => println(s"Result: $result")
    case Left(error)   => println(s"Error: $error")
  }

  // File reading with Try
  println("\n=== File Reading Example ===")
  def readFile(filePath: String): Try[String] =
    Try {
      val file = new java.io.File(filePath)
      val source = scala.io.Source.fromFile(file)
      val content = source.mkString
      source.close()
      content
    }

  readFile(".gitignore").foreach(content => println(s"Gitignore content: $content"))
  readFile("nonexistent.txt") match {
    case Success(content) => println(content)
    case Failure(e)       => println(s"Error reading file: ${e.getMessage}")
  }

  // Async ping-pong
  println("\n=== Async Example ===")
  def ping(): Future[String] =
    Future {
      Thread.sleep(1000)
      "ping"
    }

  def pong(): Future[String] =
    Future {
      Thread.sleep(1000)
      "pong"
    }

  println("Starting async operations...")
  ping().foreach(println)
  pong().foreach(println)
  
  // Wait for async operations to complete
  Thread.sleep(2500)
  println("Async operations completed")
object NameGenerator:
  private val firstNames = List("Alice", "Bob", "Charlie", "Diana", "Eve", "Frank", "Grace", "Henry")
  private val lastNames = List("Smith", "Johnson", "Williams", "Brown", "Jones", "Miller", "Davis", "Wilson")
  private val rng = new Random()
  
  private def randomElement[A](list: List[A]): A = 
    list(rng.nextInt(list.length))
    
  private def randomVowel: Char = 
    "aeiou"(rng.nextInt(5))
    
  private def randomConsonant: Char =
    "bcdfghjklmnpqrstvwxyz"(rng.nextInt(21))
    
  private def generateRandomName(length: Int): String =
    (1 to length).map { i =>
      if i % 2 == 0 then randomVowel
      else randomConsonant
    }.mkString.capitalize
    
  def generateName(): String =
    val firstName = randomElement(firstNames)
    val lastName = randomElement(lastNames)
    val middleName = generateRandomName(rng.nextInt(3) + 4)
    
    // Randomly format the name in different ways
    rng.nextInt(4) match {
      case 0 => s"$firstName $lastName"
      case 1 => s"${firstName(0)}. $lastName"
      case 2 => s"$firstName \"$middleName\" $lastName"
      case 3 => s"${lastName}, ${firstName}"
    }
  end generateName
object Ackermann:
  /**
   * The Ackermann function is a classic example of a recursive function that grows very quickly.
   * It's used in computability theory to show that not all total computable functions are primitive recursive.
   * 
   * @param m A non-negative integer
   * @param n A non-negative integer
   * @return The result of the Ackermann function for inputs m and n
   */
  def ackermann(m: Int, n: Int): Int =
    if m < 0 || n < 0 then
      throw new IllegalArgumentException("Ackermann function is only defined for non-negative integers")
    else if m == 0 then 
      n + 1
    else if n == 0 then 
      ackermann(m - 1, 1)
    else 
      ackermann(m - 1, ackermann(m, n - 1))
    
  /**
   * A memoized version of the Ackermann function to improve performance
   * by avoiding redundant calculations.
   */
  def memoizedAckermann(m: Int, n: Int): Int =
    val cache = scala.collection.mutable.Map.empty[(Int, Int), Int]
    
    def ack(m: Int, n: Int): Int = 
      cache.getOrElseUpdate((m, n), 
        if m == 0 then n + 1
        else if n == 0 then ack(m - 1, 1)
        else ack(m - 1, ack(m, n - 1))
      )
      
    ack(m, n)