package com.half.javalearning.threads;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

// Instead of this mess, try this cleaner approach:
public class ThreadMonitor {
    private static final Logger logger = Logger.getLogger(ThreadMonitor.class.getName());

    public static void main(String[] args) {
        ExecutorService executor = Executors.newFixedThreadPool(4);
        List<Future<?>> futures = new ArrayList<>();

        // Create tasks without the debug spam
        for (int i = 0; i < 10; i++) {
            SafeThreadTask task = new SafeThreadTask(i * 10 + 50); // Give them different work loads
            futures.add(executor.submit(task));
        }

        // Monitor without spamming console
        monitorTasks(futures);

        executor.shutdown();
        try {
            executor.awaitTermination(30, TimeUnit.SECONDS);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }

    private static void monitorTasks(List<Future<?>> futures) {
        int completed = 0;
        while (completed < futures.size()) {
            completed = 0;
            for (Future<?> future : futures) {
                if (future.isDone()) completed++;
            }

            logger.info(String.format("Progress: %d/%d tasks completed", completed, futures.size()));

            try {
                Thread.sleep(1000); // Check every second, not every millisecond
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                break;
            }
        }
    }
}
