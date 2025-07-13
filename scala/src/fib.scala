def fibonacci(n: Int): Int = {
  println(s"Computing fibonacci($n)")
  if (n < 2) n
  else fibonacci(n - 1) + fibonacci(n - 2)
}

@main def run(): Unit = println("Result: " + fibonacci(5))
