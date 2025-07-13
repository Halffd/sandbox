package main

import "fmt"

func main() {
    // Declare and initialize dummy variables
    dummyInt := 0
    dummyFloat := float32(0.0)
    
    // Create pointers (Go style)
    p := &dummyInt     // *int pointer
    ptr1 := &dummyFloat // *float32 pointer
    ptr2 := &dummyFloat // *float32 pointer
    
    // Modify values through pointers
    *p = 42
    *ptr1 = 3.14
    *ptr2 = 2.718
    
    fmt.Println("Pointer values:")
    fmt.Printf("p: %p -> %d\n", p, *p)
    fmt.Printf("ptr1: %p -> %f\n", ptr1, *ptr1)
    fmt.Printf("ptr2: %p -> %f\n", ptr2, *ptr2)
    
    fmt.Println("\nOriginal values:")
    fmt.Printf("dummyInt: %d\n", dummyInt)
    fmt.Printf("dummyFloat: %f\n", dummyFloat)
}