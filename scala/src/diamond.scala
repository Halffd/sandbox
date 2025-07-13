trait A { def foo(): Unit = println("A") }
trait B extends A { override def foo(): Unit = println("B") }
trait C extends A { override def foo(): Unit = println("C") }

class D extends B with C // C's implementation takes precedence

object Main extends App {
  val d = new D
  d.foo() // Output: C
}