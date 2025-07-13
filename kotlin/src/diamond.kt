interface IA { fun foo() { println("IA") } }
interface IB : IA { override fun foo() { println("IB") } }
interface IC : IA { override fun foo() { println("IC") } }

class D : IB, IC {
    override fun foo() {
        super<IB>.foo() // Explicitly choose IB's implementation
    }
}

fun main() {
    D().foo() // Output: IB
}