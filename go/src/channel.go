package main

import (
	"context"
	"fmt"
	"sync"
	"time"
)

func main() {
	ctx, cancel := context.WithCancel(context.Background())
	var wg sync.WaitGroup

	// Create channels for communication
	chan1 := make(chan string)
	chan2 := make(chan string)
	chan3 := make(chan string)

	// Start thread 1
	wg.Add(1)
	go func() {
		defer wg.Done()
		foo(chan1, ctx)
	}()

	// Start thread 2
	wg.Add(1)
	go func() {
		defer wg.Done()
		foo(chan2, ctx)
	}()

	// Start thread 3
	wg.Add(1)
	go func() {
		defer wg.Done()
		foo(chan3, ctx)
	}()

	// Broadcast messages
	wg.Add(1)
	go func() {
		defer wg.Done()
		for i := 1; i <= 3; i++ {
			time.Sleep(3 * time.Second) // Simulate work
			msg := "hello"
			chan1 <- "[chan1] " + msg
			chan2 <- "[chan2] " + msg
			chan3 <- "[chan3] " + msg
		}
		cancel() // Trigger ctx.Done()
	}()

	fmt.Println("Waiting for stuff...")
	wg.Wait()
}

func foo(a chan string, ctx context.Context) {
	for {
		select {
		case msg, ok := <-a:
			if !ok {
				return // Channel closed
			}
			fmt.Println(msg)

		case <-ctx.Done():
			fmt.Println("Exiting foo")
			return
		}
	}
}
