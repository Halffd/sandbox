package main

import "fmt"

func main() {
	var i, j int
	n := 5 // Example value for n
	A := make([]int, n+1) // Create array with n+1 elements (1-based indexing)

	// Initialize elements A[1] to A[n-1] with 0
	i = 1
	for i < n {
		A[i] = 0
		i++
	}

	// Calculate values for all elements
	for i = 1; i <= n; i++ {
		for j = 1; j <= n; j++ {
			A[i] += i * j
		}
	}

	fmt.Println("Resulting array:", A[1:]) // Print from index 1 to n
}