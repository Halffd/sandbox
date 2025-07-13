package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func main() {
	// Initialize a map to store string mappings
	mapping := make(map[string]string)

	// Create a new scanner to read user input
	scanner := bufio.NewScanner(os.Stdin)

	fmt.Println("Enter strings to add to the mapping (type 'q' to finish):")

	for {
		fmt.Print("> ")

		// Read the next line of input
		scanner.Scan()
		input := scanner.Text()

		// Check if the user wants to exit
		if strings.ToLower(input) == "q" {
			break
		}

		// Store the input in the map
		mapping[input] = input
		fmt.Printf("Added: %s\n", input)
	}

	// Display the contents of the map
	fmt.Println("Final Mapping:")
	for key, value := range mapping {
		fmt.Printf("%s: %s\n", key, value)
	}
}
