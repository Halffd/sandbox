package com.half.javalearning.threads;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicLong;
import java.util.logging.Logger;
import java.util.logging.Level;

/**
 * Thread-safe implementation using Runnable interface
 * Better than extending Thread because composition > inheritance
 */
public class SafeThreadTask implements Runnable {
    private static final Logger logger = Logger.getLogger(SafeThreadTask.class.getName());
    private static final AtomicLong threadCounter = new AtomicLong(0);

    private final long taskId;
    private final String taskName;
    private volatile boolean running = false;

    public SafeThreadTask(long taskId) {
        this.taskId = taskId;
        this.taskName = "Task-" + taskId + "-" + threadCounter.incrementAndGet();
    }

    @Override
    public void run() {
        Thread.currentThread().setName(taskName);
        running = true;

        try {
            logger.info(String.format("Starting task [%s] on thread [%s]",
                    taskName, Thread.currentThread().getName()));

            // Your actual work goes here - example with some processing
            performTask();

            logger.info(String.format("Successfully completed task [%s]", taskName));

        } catch (InterruptedException e) {
            logger.info(String.format("Task [%s] was interrupted", taskName));
            Thread.currentThread().interrupt(); // Restore interrupt status

        } catch (Exception e) {
            logger.log(Level.SEVERE,
                    String.format("Task [%s] failed with error: %s", taskName, e.getMessage()), e);

        } finally {
            running = false;
            logger.info(String.format("Task [%s] finished execution", taskName));
        }
    }

    private void performTask() throws InterruptedException {
        // Simulate some work with proper interrupt handling
        for (int i = 0; i < taskId && !Thread.currentThread().isInterrupted(); i++) {
            // Check for interruption periodically
            if (Thread.currentThread().isInterrupted()) {
                throw new InterruptedException("Task was interrupted during execution");
            }

            // Simulate work
            Thread.sleep(100);

            if (i % 10 == 0) {
                logger.fine(String.format("Task [%s] progress: %d/%d", taskName, i, taskId));
            }
        }
    }

    public boolean isRunning() {
        return running;
    }

    public String getTaskName() {
        return taskName;
    }
    public static void main(String[] args) {
        List<Thread> threads = new ArrayList<>();
        for (int i = 0; i < 10; i++) {
            SafeThreadTask task = new SafeThreadTask(i);
            Thread thread = new Thread(task);
            thread.start();
            threads.add(thread);
            Set allThreads = Thread.getAllStackTraces().keySet();
            allThreads.forEach(t -> System.out.println(t.toString() + " is " + (t.hashCode() == thread.hashCode() ? "running" : "not running")));
            var threadGroup = thread.getThreadGroup();
            List<Object> threadInfo = new ArrayList<Object>();
            threadInfo.add(Thread.currentThread());
            threadInfo.add(threadGroup.toString());
            threadInfo.add(threadGroup.getName());;
            threadInfo.add(threadGroup.getParent());
            threadInfo.add(threadGroup.getParent().getParent());
            threadInfo.add(thread.getName());
            threadInfo.add(thread.getPriority());
            threadInfo.add(thread.isAlive());
            threadInfo.add(thread.isDaemon());
            threadInfo.add(thread.isInterrupted());
            threadInfo.add(thread.getState());
            threadInfo.add(thread.getStackTrace());
            threadInfo.add(task.getTaskName());
            threadInfo.add(task.isRunning());
            threadInfo.add(task.hashCode());
            threadInfo.add(task.getClass());
            threadInfo.add(task.toString());
            threadInfo.add(task.taskId);
            threadInfo.forEach(System.out::println);
        }
        try {
            Thread.sleep(1000);
            threads.forEach(Thread::interrupt);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }
}

