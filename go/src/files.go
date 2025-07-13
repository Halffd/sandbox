package main

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"os"
	"strings"
)

func main() {
	// Open file with read/write permissions, create if not exists
	file, err := os.OpenFile("demo.txt", os.O_RDWR|os.O_CREATE, 0644)
	if err != nil {
		log.Fatal(err)
	}
	defer func() {
		if err := file.Close(); err != nil {
			log.Fatal(err)
		}
	}()

	// Get file size
	fileInfo, err := file.Stat()
	if err != nil {
		log.Fatal(err)
	}
	fmt.Printf("Initial file size: %d bytes\n", fileInfo.Size())

	// Seek to beginning of file
	_, err = file.Seek(0, io.SeekStart)
	if err != nil {
		log.Fatal(err)
	}

	// Read current content
	content, err := io.ReadAll(file)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Printf("\nCurrent content:\n%s\n", content)

	// Format content (example: convert to uppercase)
	formatted := strings.ToUpper(string(content))
	fmt.Printf("\nFormatted content:\n%s\n", formatted)

	// Seek to end to append new content
	_, err = file.Seek(0, io.SeekEnd)
	if err != nil {
		log.Fatal(err)
	}

	// Get user input
	fmt.Print("\nEnter text to append: ")
	reader := bufio.NewReader(os.Stdin)
	input, _ := reader.ReadString('\n')

	// Write user input to file
	bytesWritten, err := file.WriteString(input)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Printf("Wrote %d bytes\n", bytesWritten)

	// Flush writes to disk
	err = file.Sync()
	if err != nil {
		log.Fatal(err)
	}
	// Replace the truncation code with:
    currentSize := fileInfo.Size()
    if currentSize > 100 {
        err = file.Truncate(100)
        if err != nil {
            log.Fatal(err)
        }
    } else {
        fmt.Printf("\nFile size (%d bytes) under limit - no truncation needed\n", currentSize)
    }
    // Get final file size
	fileInfo, err = file.Stat()
	if err != nil {
		log.Fatal(err)
	}
	fmt.Printf("\nFinal file size: %d bytes\n", fileInfo.Size())
}

