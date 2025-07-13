package com.half.javalearning.video;

import javax.sound.sampled.*;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.concurrent.*;
import java.util.stream.Stream;

public class AudioPlayer {
    private static final ExecutorService executor = Executors.newVirtualThreadPerTaskExecutor();
    private static volatile boolean isPlaying = false;
    private static volatile boolean shouldLoop = false;
    private static Clip currentClip;
    private static List<File> playlist = new ArrayList<>();
    private static int currentTrackIndex = 0;
    private static Future<?> playbackTask;

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);

        System.out.println("Enter file path or directory path: ");
        String path = scanner.nextLine();

        if (path.isEmpty()) {
            System.out.println("Invalid path");
            return;
        }

        loadAudioFiles(path);

        if (playlist.isEmpty()) {
            System.out.println("No valid audio files found");
            return;
        }

        System.out.println("Loaded " + playlist.size() + " audio files");

        // Start playback control loop
        controlLoop(scanner);

        executor.shutdown();
    }

    private static void loadAudioFiles(String path) {
        File file = new File(path);

        if (!file.exists()) {
            System.out.println("Path doesn't exist");
            return;
        }

        if (file.isFile()) {
            if (isValidAudioFile(file)) {
                playlist.add(file);
            } else {
                System.out.println("Invalid audio file type");
            }
        } else if (file.isDirectory()) {
            try (Stream<Path> paths = Files.walk(Paths.get(path))) {
                paths.filter(Files::isRegularFile)
                        .map(Path::toFile)
                        .filter(AudioPlayer::isValidAudioFile)
                        .forEach(playlist::add);
            } catch (IOException e) {
                System.out.println("Error reading directory: " + e.getMessage());
            }
        }
    }

    private static boolean isValidAudioFile(File file) {
        String name = file.getName().toLowerCase();
        return name.endsWith(".wav") || name.endsWith(".au") || name.endsWith(".aiff");
    }

    private static void controlLoop(Scanner scanner) {
        String response = "";

        while (!response.equals("Q")) {
            printMenu();

            if (!scanner.hasNext()) {
                break;
            }

            response = scanner.next().toUpperCase();

            switch (response) {
                case "P" -> playCurrentTrack();
                case "S" -> stopPlayback();
                case "R" -> resetTrack();
                case "N" -> nextTrack();
                case "B" -> previousTrack();
                case "L" -> toggleLoop();
                case "LIST" -> showPlaylist();
                case "Q" -> quit();
                default -> System.out.println("Invalid choice");
            }
        }
    }

    private static void printMenu() {
        System.out.println("\n=== Audio Player Controls ===");
        System.out.println("P = Play");
        System.out.println("S = Stop");
        System.out.println("R = Reset");
        System.out.println("N = Next Track");
        System.out.println("B = Previous Track");
        System.out.println("L = Toggle Loop " + (shouldLoop ? "(ON)" : "(OFF)"));
        System.out.println("LIST = Show Playlist");
        System.out.println("Q = Quit");
        System.out.println("Currently: " + (currentTrackIndex + 1) + "/" + playlist.size() + " - " +
                playlist.get(currentTrackIndex).getName());
        System.out.print("Enter your choice: ");
    }

    private static void playCurrentTrack() {
        if (isPlaying) {
            System.out.println("Already playing");
            return;
        }

        // Cancel any existing playback task
        if (playbackTask != null && !playbackTask.isDone()) {
            playbackTask.cancel(true);
        }

        playbackTask = executor.submit(() -> {
            try {
                File currentFile = playlist.get(currentTrackIndex);
                System.out.println("Playing: " + currentFile.getName());

                if (currentClip != null) {
                    currentClip.close();
                }

                AudioInputStream audioStream = AudioSystem.getAudioInputStream(currentFile);
                currentClip = AudioSystem.getClip();
                currentClip.open(audioStream);

                // Add loop listener
                currentClip.addLineListener(event -> {
                    if (event.getType() == LineEvent.Type.STOP) {
                        isPlaying = false;
                        if (shouldLoop) {
                            // Loop current track
                            currentClip.setFramePosition(0);
                            currentClip.start();
                            isPlaying = true;
                        } else if (currentTrackIndex < playlist.size() - 1) {
                            // Auto-advance to next track
                            currentTrackIndex++;
                            playCurrentTrack();
                        }
                    }
                });

                isPlaying = true;
                currentClip.start();

            } catch (UnsupportedAudioFileException e) {
                System.out.println("Unsupported audio format");
            } catch (LineUnavailableException e) {
                System.out.println("Audio line unavailable");
            } catch (IOException e) {
                System.out.println("Error reading file: " + e.getMessage());
            }
        });
    }

    private static void stopPlayback() {
        if (currentClip != null) {
            currentClip.stop();
        }
        isPlaying = false;
        if (playbackTask != null) {
            playbackTask.cancel(true);
        }
        System.out.println("Stopped");
    }

    private static void resetTrack() {
        if (currentClip != null) {
            currentClip.setFramePosition(0);
            System.out.println("Reset to beginning");
        }
    }

    private static void nextTrack() {
        if (currentTrackIndex < playlist.size() - 1) {
            currentTrackIndex++;
            stopPlayback();
            System.out.println("Next track: " + playlist.get(currentTrackIndex).getName());
        } else {
            System.out.println("Already at last track");
        }
    }

    private static void previousTrack() {
        if (currentTrackIndex > 0) {
            currentTrackIndex--;
            stopPlayback();
            System.out.println("Previous track: " + playlist.get(currentTrackIndex).getName());
        } else {
            System.out.println("Already at first track");
        }
    }

    private static void toggleLoop() {
        shouldLoop = !shouldLoop;
        System.out.println("Loop " + (shouldLoop ? "enabled" : "disabled"));
    }

    private static void showPlaylist() {
        System.out.println("\n=== Playlist ===");
        for (int i = 0; i < playlist.size(); i++) {
            String marker = (i == currentTrackIndex) ? "▶ " : "  ";
            System.out.println(marker + (i + 1) + ". " + playlist.get(i).getName());
        }
    }

    private static void quit() {
        stopPlayback();
        if (currentClip != null) {
            currentClip.close();
        }
        System.out.println("Bye!");
    }
}