package main

import (
	"fmt"
	"math"
	"math/rand"
	"runtime"
	"sync/atomic"
	"time"
)

var (
	totalThreads     int
	maxThreadLimit   = 65536 // Theoretical max thread count
	globalStopSignal atomic.Bool
)

func intensiveComputeTask(taskID int, done chan struct{}) {
	defer func() {
		done <- struct{}{}
	}()

	// Combination of compute-intensive operations
	localRand := rand.New(rand.NewSource(time.Now().UnixNano() + int64(taskID)))
	
	for globalStopSignal.Load() == false {
		// Matrix-like multiplication
		a := localRand.Float64() * 1e6
		b := localRand.Float64() * 1e6
		_ = a * b

		// Trigonometric complexity
		x := math.Sin(a) * math.Cos(b)
		y := math.Exp(x)
		z := math.Log(math.Abs(y) + 1)

		// Prime-like computation
		candidate := int(math.Abs(z * 1e6))
		isPrime := candidate > 1
		for i := 2; i*i <= candidate; i++ {
			if candidate%i == 0 {
				isPrime = false
				break
			}
		}

		// Prevent compiler optimizations
		if isPrime {
			_ = candidate * candidate
		}
	}
}

func maximumCPUStress() {
	// Determine maximum possible threads
	numCPU := runtime.NumCPU()
	runtime.GOMAXPROCS(numCPU)

	// Calculate total threads: CPU cores * aggressive multiplier
	totalThreads = numCPU * 128 // Aggressive thread multiplication
	if totalThreads > maxThreadLimit {
		totalThreads = maxThreadLimit
	}

	fmt.Printf("Maximum CPU Stress Parameters:\n")
	fmt.Printf("Total CPU Cores:    %d\n", numCPU)
	fmt.Printf("Total Threads:      %d\n", totalThreads)
	fmt.Printf("Max Proc Setting:   %d\n", numCPU)

	// Prepare synchronization
	globalStopSignal.Store(false)
	done := make(chan struct{}, totalThreads)
	
	// Launch maximum threads
	for i := 0; i < totalThreads; i++ {
		go intensiveComputeTask(i, done)
	}

	// Wait for user interrupt
	fmt.Println("\nStress test running. Press Enter to stop...")
	fmt.Scanln()

	// Signal all threads to stop
	globalStopSignal.Store(true)

	// Wait for all threads to acknowledge
	for i := 0; i < totalThreads; i++ {
		<-done
	}

	fmt.Println("Maximum CPU stress test completed safely.")
}

func main() {
	// Disable GC to reduce overhead
	debug := runtime.MemProfileRate
	runtime.MemProfileRate = 0
	defer func() { runtime.MemProfileRate = debug }()

	// Seed randomness
	rand.Seed(time.Now().UnixNano())

	// Run stress test
	maximumCPUStress()
}