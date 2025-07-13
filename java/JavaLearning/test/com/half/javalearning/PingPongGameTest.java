package com.half.javalearning;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;
import static org.junit.jupiter.api.Assertions.*;
import com.half.javalearning.threads.*
/**
 * Unit tests for the PingPong game components.
 */
public class PingPongGameTest {

    @Test
    @Timeout(10)
    public void testBasicGameCompletion() {
        PingPongGame game = new PingPongGame(3);

        game.startGame();
        boolean completed = game.waitForGameCompletion(5);

        assertTrue(completed, "Game should complete within timeout");
        assertTrue(game.getGameStats().completed, "Game should be marked as completed");
        assertEquals(3, game.getGameStats().currentRound, "Should complete all rounds");
    }

    @Test
    @Timeout(5)
    public void testGameEarlyStop() {
        PingPongGame game = new PingPongGame(100); // Long game

        game.startGame();

        // Let it run briefly then stop
        try {
            Thread.sleep(500);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }

        game.stopGame();

        assertTrue(game.getGameStats().stopped, "Game should be marked as stopped");
        assertFalse(game.getGameStats().completed, "Game should not be completed");
    }

    @Test
    public void testInvalidRounds() {
        assertThrows(IllegalArgumentException.class, () -> new PingPongGame(0));
        assertThrows(IllegalArgumentException.class, () -> new PingPongGame(-1));
    }

    @Test
    public void testMultipleStartAttempts() {
        PingPongGame game = new PingPongGame(2);

        game.startGame();

        // Second start should fail
        assertThrows(IllegalStateException.class, () -> game.startGame());
    }

    @Test
    public void testGameController() {
        GameController controller = new GameController(5);

        assertEquals(0, controller.getCurrentRound());
        assertEquals(5, controller.getMaxRounds());
        assertTrue(controller.shouldContinue());

        // Increment rounds
        for (int i = 0; i < 4; i++) {
            assertTrue(controller.incrementRound());
            assertEquals(i + 1, controller.getCurrentRound());
        }

        // Last round should complete the game
        assertFalse(controller.incrementRound());
        assertTrue(controller.isGameComplete());
        assertFalse(controller.shouldContinue());
    }
}