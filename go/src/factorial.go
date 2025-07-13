package main

import "fmt"

func fatorial(n int) int {
    aux := 1
    fmt.Printf("Calculating factorial of %d\n", n)
    for i := 1; i <= n; i++ {
        aux *= i
        fmt.Printf("Step %d: Multiply by %d → Result: %d\n", i, i, aux)
    }
    return aux
}

func main() {
    result := fatorial(5)
    fmt.Println("Final Result:", result)
}