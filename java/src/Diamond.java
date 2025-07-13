public class Diamond {
    interface IA { default void foo() { System.out.println("IA"); } }
    interface IB extends IA { default void foo() { System.out.println("IB"); } }
    interface IC extends IA { default void foo() { System.out.println("IC"); } }

    static class D implements IB, IC {
        @Override
        public void foo() { // Must override to resolve ambiguity
            IB.super.foo(); // Explicitly choose IB's implementation
        }
    }

    public static void main(String[] args) {
        D d = new D();
        d.foo(); // Output: IB
    }
}