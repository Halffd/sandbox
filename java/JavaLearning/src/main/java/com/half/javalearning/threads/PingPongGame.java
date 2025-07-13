package com.half.javalearning.threads;

import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.logging.Logger;

/**
 * Thread-safe game controller that manages the state and progress of the PingPong game.
 * Handles round counting, game completion, and forced termination.
 */
class GameController {
    private static final Logger LOGGER = Logger.getLogger(GameController.class.getName());

    private final AtomicInteger currentRound = new AtomicInteger(0);
    private final AtomicInteger maxRounds;
    private final AtomicBoolean gameComplete = new AtomicBoolean(false);
    private final AtomicBoolean gameStopped = new AtomicBoolean(false);

    /**
     * Creates a new game controller with specified maximum rounds.
     *
     * @param maxRounds Maximum number of rounds for the game
     * @throws IllegalArgumentException if maxRounds is not positive
     */
    public GameController(int maxRounds) {
        if (maxRounds <= 0) {
            throw new IllegalArgumentException("Max rounds must be positive, got: " + maxRounds);
        }
        this.maxRounds = new AtomicInteger(maxRounds);
        LOGGER.info(String.format("GameController initialized with %d max rounds", maxRounds));
    }

    /**
     * Increments the round counter and checks for game completion.
     * This method is thread-safe.
     *
     * @return true if game should continue, false if game is complete or stopped
     */
    public boolean incrementRound() {
        if (gameStopped.get()) {
            return false;
        }

        int newRound = currentRound.incrementAndGet();

        if (newRound >= maxRounds.get()) {
            gameComplete.set(true);
            LOGGER.info("Game completed after " + newRound + " rounds");
            return false;
        }

        LOGGER.fine(String.format("Round %d/%d completed", newRound, maxRounds.get()));
        return true;
    }

    /**
     * Gets the current round number.
     *
     * @return Current round number (0-based)
     */
    public int getCurrentRound() {
        return currentRound.get();
    }

    /**
     * Gets the maximum number of rounds.
     *
     * @return Maximum rounds
     */
    public int getMaxRounds() {
        return maxRounds.get();
    }

    /**
     * Checks if the game is complete.
     *
     * @return true if all rounds have been played
     */
    public boolean isGameComplete() {
        return gameComplete.get();
    }

    /**
     * Checks if the game has been forcefully stopped.
     *
     * @return true if game was stopped before completion
     */
    public boolean isGameStopped() {
        return gameStopped.get();
    }

    /**
     * Forcefully stops the game.
     */
    public void stopGame() {
        gameStopped.set(true);
        LOGGER.info("Game forcefully stopped at round " + currentRound.get());
    }

    /**
     * Checks if the game should continue running.
     *
     * @return true if game should continue, false otherwise
     */
    public boolean shouldContinue() {
        return !gameComplete.get() && !gameStopped.get();
    }

    /**
     * Gets the remaining rounds.
     *
     * @return Number of rounds remaining
     */
    public int getRemainingRounds() {
        return Math.max(0, maxRounds.get() - currentRound.get());
    }
}

/**
 * PingPlayer represents one side of the ping-pong game.
 * Waits for its turn, plays "PING", then signals the PongPlayer.
 */
class PingPlayer implements Runnable {
    private static final Logger LOGGER = Logger.getLogger(PingPlayer.class.getName());
    private static final long SEMAPHORE_TIMEOUT_SECONDS = 10;

    private final Semaphore pingSemaphore;
    private final Semaphore pongSemaphore;
    private final GameController gameController;

    /**
     * Creates a new PingPlayer.
     *
     * @param pingSemaphore Semaphore to wait on for ping turn
     * @param pongSemaphore Semaphore to signal when ping is complete
     * @param gameController Controller to manage game state
     */
    public PingPlayer(Semaphore pingSemaphore, Semaphore pongSemaphore, GameController gameController) {
        this.pingSemaphore = pingSemaphore;
        this.pongSemaphore = pongSemaphore;
        this.gameController = gameController;
    }

    @Override
    public void run() {
        LOGGER.info("PingPlayer started");

        try {
            while (gameController.shouldContinue()) {
                // Wait for permission to play ping
                boolean acquired = pingSemaphore.tryAcquire(SEMAPHORE_TIMEOUT_SECONDS, TimeUnit.SECONDS);

                if (!acquired) {
                    if (gameController.shouldContinue()) {
                        LOGGER.warning("PingPlayer timed out waiting for semaphore");
                        continue;
                    } else {
                        break; // Game ended while waiting
                    }
                }

                // Double-check game state after acquiring semaphore
                if (!gameController.shouldContinue()) {
                    break;
                }

                // Play the ping
                playPing();

                // Signal pong player (but only if game should continue)
                if (gameController.shouldContinue()) {
                    pongSemaphore.release();
                }
            }

        } catch (InterruptedException e) {
            LOGGER.info("PingPlayer interrupted, shutting down gracefully");
            Thread.currentThread().interrupt();
        } catch (Exception e) {
            LOGGER.log(Level.SEVERE, "Unexpected error in PingPlayer", e);
        }

        LOGGER.info("PingPlayer finished");
    }

    /**
     * Performs the actual "ping" action.
     * This method can be overridden for different ping behaviors.
     */
    protected void playPing() {
        int currentRound = gameController.getCurrentRound();
        System.out.printf("[Round %d] PING%n", currentRound + 1);
        LOGGER.fine(String.format("PING played at round %d", currentRound + 1));

        // Simulate some processing time
        try {
            Thread.sleep(50); // Small delay to make output more readable
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new RuntimeException("Interrupted during ping", e);
        }
    }

    /**
     * Gets the name of this player for logging purposes.
     *
     * @return Player name
     */
    public String getPlayerName() {
        return "PingPlayer";
    }
}
/**
 * PongPlayer represents the other side of the ping-pong game.
 * Waits for the PingPlayer to play, then responds with "PONG".
 */
class PongPlayer implements Runnable {
    private static final Logger LOGGER = Logger.getLogger(PongPlayer.class.getName());
    private static final long SEMAPHORE_TIMEOUT_SECONDS = 10;

    private final Semaphore pingSemaphore;
    private final Semaphore pongSemaphore;
    private final GameController gameController;

    /**
     * Creates a new PongPlayer.
     *
     * @param pingSemaphore Semaphore to signal when pong is complete
     * @param pongSemaphore Semaphore to wait on for pong turn
     * @param gameController Controller to manage game state
     */
    public PongPlayer(Semaphore pingSemaphore, Semaphore pongSemaphore, GameController gameController) {
        this.pingSemaphore = pingSemaphore;
        this.pongSemaphore = pongSemaphore;
        this.gameController = gameController;
    }

    @Override
    public void run() {
        LOGGER.info("PongPlayer started");

        try {
            while (gameController.shouldContinue()) {
                // Wait for permission to play pong
                boolean acquired = pongSemaphore.tryAcquire(SEMAPHORE_TIMEOUT_SECONDS, TimeUnit.SECONDS);

                if (!acquired) {
                    if (gameController.shouldContinue()) {
                        LOGGER.warning("PongPlayer timed out waiting for semaphore");
                        continue;
                    } else {
                        break; // Game ended while waiting
                    }
                }

                // Double-check game state after acquiring semaphore
                if (!gameController.shouldContinue()) {
                    break;
                }

                // Play the pong
                playPong();

                // Increment round after pong (completes the exchange)
                boolean shouldContinue = gameController.incrementRound();

                // Signal ping player for next round (but only if game should continue)
                if (shouldContinue) {
                    pingSemaphore.release();
                }
            }

        } catch (InterruptedException e) {
            LOGGER.info("PongPlayer interrupted, shutting down gracefully");
            Thread.currentThread().interrupt();
        } catch (Exception e) {
            LOGGER.log(Level.SEVERE, "Unexpected error in PongPlayer", e);
        }

        LOGGER.info("PongPlayer finished");
    }

    /**
     * Performs the actual "pong" action.
     * This method can be overridden for different pong behaviors.
     */
    protected void playPong() {
        int currentRound = gameController.getCurrentRound();
        System.out.printf("[Round %d] PONG%n", currentRound + 1);
        LOGGER.fine(String.format("PONG played at round %d", currentRound + 1));

        // Simulate some processing time
        try {
            Thread.sleep(50); // Small delay to make output more readable
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new RuntimeException("Interrupted during pong", e);
        }
    }

    /**
     * Gets the name of this player for logging purposes.
     *
     * @return Player name
     */
    public String getPlayerName() {
        return "PongPlayer";
    }
}

/**
 * Thread-safe PingPong game coordinator using semaphores for synchronization.
 * Manages the lifecycle of ping and pong threads with proper resource management.
 */
public class PingPongGame {
    private static final Logger LOGGER = Logger.getLogger(PingPongGame.class.getName());

    // Semaphores for thread coordination
    private final Semaphore pingSemaphore;
    private final Semaphore pongSemaphore;

    // Game components
    private final GameController gameController;
    private final PingPlayer pingPlayer;
    private final PongPlayer pongPlayer;

    // Thread management
    private Thread pingThread;
    private Thread pongThread;
    private final AtomicBoolean gameStarted = new AtomicBoolean(false);
    private final AtomicBoolean gameStopped = new AtomicBoolean(false);

    // Game configuration
    private final int maxRounds;

    /**
     * Creates a new PingPong game with specified number of rounds.
     *
     * @param maxRounds Maximum number of ping-pong exchanges
     * @throws IllegalArgumentException if maxRounds is not positive
     */
    public PingPongGame(int maxRounds) {
        if (maxRounds <= 0) {
            throw new IllegalArgumentException("Game rounds must be positive, got: " + maxRounds);
        }

        this.maxRounds = maxRounds;

        // Initialize semaphores - both start at 0 to ensure controlled start
        this.pingSemaphore = new Semaphore(0, true); // Fair ordering
        this.pongSemaphore = new Semaphore(0, true);

        // Initialize game controller
        this.gameController = new GameController(maxRounds);

        // Initialize players with semaphores and controller
        this.pingPlayer = new PingPlayer(pingSemaphore, pongSemaphore, gameController);
        this.pongPlayer = new PongPlayer(pingSemaphore, pongSemaphore, gameController);

        LOGGER.info(String.format("PingPong game created with %d rounds", maxRounds));
    }

    /**
     * Starts the ping-pong game.
     * This method is thread-safe and can only start the game once.
     *
     * @throws IllegalStateException if game is already started or stopped
     */
    public synchronized void startGame() {
        if (gameStarted.get()) {
            throw new IllegalStateException("Game is already started");
        }

        if (gameStopped.get()) {
            throw new IllegalStateException("Game has been stopped and cannot be restarted");
        }

        try {
            LOGGER.info("Starting PingPong game...");

            // Create and configure threads
            pingThread = new Thread(pingPlayer, "PingThread");
            pongThread = new Thread(pongPlayer, "PongThread");

            // Set as daemon threads so they don't prevent JVM shutdown
            pingThread.setDaemon(true);
            pongThread.setDaemon(true);

            // Start threads
            pingThread.start();
            pongThread.start();

            // Mark game as started
            gameStarted.set(true);

            // Give small delay to ensure threads are ready
            Thread.sleep(100);

            // Release the first semaphore to start the ping-pong sequence
            pingSemaphore.release();

            LOGGER.info("PingPong game started successfully");

        } catch (InterruptedException e) {
            LOGGER.log(Level.WARNING, "Game start was interrupted", e);
            Thread.currentThread().interrupt();
            throw new RuntimeException("Failed to start game due to interruption", e);
        } catch (Exception e) {
            LOGGER.log(Level.SEVERE, "Failed to start game", e);
            throw new RuntimeException("Failed to start game", e);
        }
    }

    /**
     * Waits for the game to complete with a timeout.
     *
     * @param timeoutSeconds Maximum time to wait for game completion
     * @return true if game completed within timeout, false otherwise
     */
    /**
     * Waits for the game to complete with a timeout.
     *
     * @param timeoutSeconds Maximum time to wait for game completion
     * @return true if game completed within timeout, false otherwise
     */
    public boolean waitForGameCompletion(long timeoutSeconds) {
        if (!gameStarted.get()) {
            LOGGER.warning("Game not started, cannot wait for completion");
            return false;
        }

        try {
            long halfTimeout = TimeUnit.SECONDS.toMillis(timeoutSeconds / 2);

            // Wait for ping thread to finish
            pingThread.join(halfTimeout);
            boolean pingCompleted = !pingThread.isAlive();

            // Wait for pong thread to finish
            pongThread.join(halfTimeout);
            boolean pongCompleted = !pongThread.isAlive();

            boolean completed = pingCompleted && pongCompleted;

            if (completed) {
                LOGGER.info("Game completed successfully");
            } else {
                LOGGER.warning("Game did not complete within timeout");
            }

            return completed;

        } catch (InterruptedException e) {
            LOGGER.log(Level.WARNING, "Interrupted while waiting for game completion", e);
            Thread.currentThread().interrupt();
            return false;
        }
    }
    /**
     * Stops the game forcefully.
     * This method is thread-safe and can be called multiple times.
     */
    public synchronized void stopGame() {
        if (gameStopped.get()) {
            return; // Already stopped
        }

        LOGGER.info("Stopping PingPong game...");

        gameStopped.set(true);
        gameController.stopGame();

        // Interrupt threads if they're still running
        if (pingThread != null && pingThread.isAlive()) {
            pingThread.interrupt();
        }

        if (pongThread != null && pongThread.isAlive()) {
            pongThread.interrupt();
        }

        // Release semaphores to unblock any waiting threads
        pingSemaphore.release(10); // Release multiple permits to ensure unblocking
        pongSemaphore.release(10);

        LOGGER.info("PingPong game stopped");
    }

    /**
     * Gets the current game statistics.
     *
     * @return GameStats object with current game information
     */
    public GameStats getGameStats() {
        return new GameStats(
                gameController.getCurrentRound(),
                maxRounds,
                gameStarted.get(),
                gameStopped.get(),
                gameController.isGameComplete()
        );
    }

    /**
     * Checks if the game is currently running.
     *
     * @return true if game is started but not stopped or completed
     */
    public boolean isGameRunning() {
        return gameStarted.get() && !gameStopped.get() && !gameController.isGameComplete();
    }

    /**
     * Gets the available permits for monitoring semaphore state.
     *
     * @return String representation of semaphore states
     */
    public String getSemaphoreStatus() {
        return String.format("Ping permits: %d, Pong permits: %d",
                pingSemaphore.availablePermits(),
                pongSemaphore.availablePermits());
    }

    /**
     * Container class for game statistics.
     */
    public static class GameStats {
        public final int currentRound;
        public final int maxRounds;
        public final boolean started;
        public final boolean stopped;
        public final boolean completed;

        public GameStats(int currentRound, int maxRounds, boolean started, boolean stopped, boolean completed) {
            this.currentRound = currentRound;
            this.maxRounds = maxRounds;
            this.started = started;
            this.stopped = stopped;
            this.completed = completed;
        }

        @Override
        public String toString() {
            return String.format("GameStats{round=%d/%d, started=%s, stopped=%s, completed=%s}",
                    currentRound, maxRounds, started, stopped, completed);
        }
    }
    public static void main(String[] args) {
        PingPongGame game = new PingPongGame(5);
        game.startGame();
    }
}
/**
 * Main application class that demonstrates the PingPong game.
 * Handles user interaction and game monitoring.
 */
class PingPongApplication {
    private static final Logger LOGGER = Logger.getLogger(PingPongApplication.class.getName());

    public static void main(String[] args) {
        // Configure logging level
        Logger.getLogger("").setLevel(Level.INFO);

        try {
            // Create a game with 10 rounds (20 total exchanges)
            int rounds = 10;
            if (args.length > 0) {
                try {
                    rounds = Integer.parseInt(args[0]);
                    if (rounds <= 0) {
                        throw new IllegalArgumentException("Rounds must be positive");
                    }
                } catch (NumberFormatException e) {
                    System.err.println("Invalid number format for rounds: " + args[0]);
                    System.err.println("Using default value: " + rounds);
                }
            }

            System.out.println("=== PingPong Game Starting ===");
            System.out.println("Rounds: " + rounds);
            System.out.println("Total exchanges: " + (rounds * 2));
            System.out.println();

            // Create and start the game
            PingPongGame game = new PingPongGame(rounds);

            // Start monitoring thread
            Thread monitor = createMonitorThread(game);
            monitor.start();

            // Start the game
            game.startGame();

            // Wait for completion with timeout
            long timeoutSeconds = 30;
            boolean completed = game.waitForGameCompletion(timeoutSeconds);

            if (!completed) {
                System.err.println("Game did not complete within " + timeoutSeconds + " seconds, stopping...");
                game.stopGame();
            }

            // Stop monitoring
            monitor.interrupt();

            // Print final statistics
            System.out.println();
            System.out.println("=== Game Finished ===");
            System.out.println("Final stats: " + game.getGameStats());
            System.out.println("Semaphore status: " + game.getSemaphoreStatus());

        } catch (Exception e) {
            LOGGER.log(Level.SEVERE, "Application error", e);
            System.err.println("Application failed: " + e.getMessage());
            System.exit(1);
        }
    }

    /**
     * Creates a monitoring thread that periodically reports game status.
     */
    private static Thread createMonitorThread(PingPongGame game) {
        return new Thread(() -> {
            try {
                while (!Thread.currentThread().isInterrupted() && game.isGameRunning()) {
                    Thread.sleep(2000); // Report every 2 seconds

                    PingPongGame.GameStats stats = game.getGameStats();
                    if (stats.started && !stats.completed) {
                        System.out.println("[Monitor] " + stats + " | " + game.getSemaphoreStatus());
                    }
                }
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }, "GameMonitor");
    }
}
/**
 * Builder pattern for creating PingPong games with custom configurations.
 */
class PingPongGameBuilder {
    private int maxRounds = 10;
    private String pingMessage = "PING";
    private String pongMessage = "PONG";
    private long pingDelay = 50;
    private long pongDelay = 50;
    private boolean useEnhancedPlayers = false;

    public PingPongGameBuilder rounds(int rounds) {
        if (rounds <= 0) throw new IllegalArgumentException("Rounds must be positive");
        this.maxRounds = rounds;
        return this;
    }

    public PingPongGameBuilder pingMessage(String message) {
        this.pingMessage = message;
        this.useEnhancedPlayers = true;
        return this;
    }

    public PingPongGameBuilder pongMessage(String message) {
        this.pongMessage = message;
        this.useEnhancedPlayers = true;
        return this;
    }

    public PingPongGameBuilder delays(long pingMs, long pongMs) {
        this.pingDelay = Math.max(0, pingMs);
        this.pongDelay = Math.max(0, pongMs);
        this.useEnhancedPlayers = true;
        return this;
    }

    public PingPongGame build() {
        return new PingPongGame(maxRounds);
    }
}
