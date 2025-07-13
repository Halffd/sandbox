class A:
    def foo(self):
        print("A")

class B(A):
    def foo(self):
        print("B")

class C(A):
    def foo(self):
        print("C")

class D(B, C):
    pass

d = D()
d.foo()  # Output: B (follows Method Resolution Order: D -> B -> C -> A)