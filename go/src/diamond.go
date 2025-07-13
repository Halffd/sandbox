package main

import "fmt"

type A struct{}
func (a A) Foo() { fmt.Println("A") }

type B struct{ A }
func (b B) Foo() { fmt.Println("B") }

type C struct{ A }
func (c C) Foo() { fmt.Println("C") }

type D struct{ B; C }

func main() {
    d := D{}
    d.B.Foo() // Output: B (must qualify explicitly)
    d.C.Foo() // Output: C (no default resolution)
}