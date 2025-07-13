// JavaScript uses mixins/composition instead
class A { foo() { console.log("A"); } }
class B extends A { foo() { console.log("B"); } }
class C extends A { foo() { console.log("C"); } }

// Composition-based workaround
class D {
  constructor() {
    this.b = new B();
    this.c = new C();
  }
  foo() { this.b.foo(); } // Choose implementation explicitly
}

const d = new D();
d.foo(); // Output: B