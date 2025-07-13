package com.half.javalearning.threads;

import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;
import java.util.logging.Level;

/**
 * Demonstrates safe semaphore usage with proper resource management
 * and exception handling for controlling concurrent access to shared resources.
 */
public class Semaphore {
    private static final Logger LOGGER = Logger.getLogger(Semaphore.class.getName());

    // Maximum concurrent threads allowed to access the critical section
    private static final int MAX_CONCURRENT_ACCESS = 3;

    // Semaphore with fair ordering (FIFO) to prevent thread starvation
    private final java.util.concurrent.Semaphore semaphore = new java.util.concurrent.Semaphore(MAX_CONCURRENT_ACCESS, true);

    // Shared resource counter to simulate work
    private volatile int sharedResource = 0;

    /**
     * Safely acquires a single permit and performs critical section work
     */
    public void performCriticalWork(String threadName) {
        boolean permitAcquired = false;

        try {
            LOGGER.info(String.format("Thread %s attempting to acquire permit...", threadName));

            // Try to acquire permit with timeout to avoid indefinite blocking
            permitAcquired = semaphore.tryAcquire(5, TimeUnit.SECONDS);

            if (!permitAcquired) {
                LOGGER.warning(String.format("Thread %s failed to acquire permit within timeout", threadName));
                return;
            }

            LOGGER.info(String.format("Thread %s acquired permit. Available permits: %d",
                    threadName, semaphore.availablePermits()));

            // Simulate critical section work
            performActualWork(threadName);

        } catch (InterruptedException e) {
            LOGGER.log(Level.WARNING, String.format("Thread %s was interrupted", threadName), e);
            Thread.currentThread().interrupt(); // Restore interrupt status
        } catch (Exception e) {
            LOGGER.log(Level.SEVERE, String.format("Unexpected error in thread %s", threadName), e);
        } finally {
            // Critical: Always release the permit in finally block
            if (permitAcquired) {
                semaphore.release();
                LOGGER.info(String.format("Thread %s released permit. Available permits: %d",
                        threadName, semaphore.availablePermits()));
            }
        }
    }

    /**
     * Safely acquires multiple permits for bulk operations
     */
    public void performBulkWork(String threadName, int permitsNeeded) {
        boolean permitsAcquired = false;

        try {
            LOGGER.info(String.format("Thread %s attempting to acquire %d permits...",
                    threadName, permitsNeeded));

            // Validate permits request
            if (permitsNeeded <= 0 || permitsNeeded > MAX_CONCURRENT_ACCESS) {
                throw new IllegalArgumentException("Invalid number of permits requested: " + permitsNeeded);
            }

            permitsAcquired = semaphore.tryAcquire(permitsNeeded, 10, TimeUnit.SECONDS);

            if (!permitsAcquired) {
                LOGGER.warning(String.format("Thread %s failed to acquire %d permits within timeout",
                        threadName, permitsNeeded));
                return;
            }

            LOGGER.info(String.format("Thread %s acquired %d permits. Available permits: %d",
                    threadName, permitsNeeded, semaphore.availablePermits()));

            // Simulate bulk work
            performActualBulkWork(threadName, permitsNeeded);

        } catch (InterruptedException e) {
            LOGGER.log(Level.WARNING, String.format("Thread %s was interrupted", threadName), e);
            Thread.currentThread().interrupt();
        } catch (Exception e) {
            LOGGER.log(Level.SEVERE, String.format("Unexpected error in thread %s", threadName), e);
        } finally {
            if (permitsAcquired) {
                semaphore.release(permitsNeeded);
                LOGGER.info(String.format("Thread %s released %d permits. Available permits: %d",
                        threadName, permitsNeeded, semaphore.availablePermits()));
            }
        }
    }

    /**
     * Simulates actual work being done in the critical section
     */
    private void performActualWork(String threadName) throws InterruptedException {
        // Increment shared resource safely (this is just for demo)
        int currentValue = ++sharedResource;

        LOGGER.info(String.format("Thread %s working on shared resource. Value: %d",
                threadName, currentValue));

        // Simulate some processing time
        Thread.sleep(ThreadLocalRandom.current().nextInt(1000, 3000));

        LOGGER.info(String.format("Thread %s completed work", threadName));
    }

    /**
     * Simulates bulk work requiring multiple permits
     */
    private void performActualBulkWork(String threadName, int permits) throws InterruptedException {
        LOGGER.info(String.format("Thread %s performing bulk work with %d permits", threadName, permits));

        // Simulate more intensive work
        Thread.sleep(ThreadLocalRandom.current().nextInt(2000, 4000));

        LOGGER.info(String.format("Thread %s completed bulk work", threadName));
    }

    /**
     * Gets current semaphore status for monitoring
     */
    public String getSemaphoreStatus() {
        return String.format("Available permits: %d, Queued threads: %d",
                semaphore.availablePermits(),
                semaphore.getQueueLength());
    }

    /**
     * Demonstration of proper semaphore usage
     */
    public static void main(String[] args) {
        Semaphore example = new Semaphore();

        LOGGER.info("Starting semaphore demonstration with " + MAX_CONCURRENT_ACCESS + " max concurrent access");

        // Create multiple threads to demonstrate concurrent access control
        Thread[] workers = new Thread[8];

        for (int i = 0; i < workers.length; i++) {
            final String threadName = "Worker-" + (i + 1);

            if (i < 6) {
                // Most threads do single permit work
                workers[i] = new Thread(() -> example.performCriticalWork(threadName));
            } else {
                // Last two threads do bulk work
                workers[i] = new Thread(() -> example.performBulkWork(threadName, 2));
            }

            workers[i].setName(threadName);
        }

        // Start all threads
        for (Thread worker : workers) {
            worker.start();
        }

        // Monitor semaphore status
        Thread monitor = new Thread(() -> {
            try {
                for (int i = 0; i < 20; i++) {
                    Thread.sleep(1000);
                    LOGGER.info("Semaphore Status: " + example.getSemaphoreStatus());
                }
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        });
        monitor.setName("Monitor");
        monitor.start();

        // Wait for all workers to complete
        for (Thread worker : workers) {
            try {
                worker.join();
            } catch (InterruptedException e) {
                LOGGER.log(Level.WARNING, "Main thread interrupted while waiting for workers", e);
                Thread.currentThread().interrupt();
                break;
            }
        }

        try {
            monitor.join();
        } catch (InterruptedException e) {
            LOGGER.log(Level.WARNING, "Main thread interrupted while waiting for monitor", e);
        }

        LOGGER.info("Final shared resource value: " + example.sharedResource);
        LOGGER.info("Demonstration completed");
    }
}