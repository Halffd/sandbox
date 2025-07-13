package com.half.javalearning.ui;

import javax.swing.*;
import java.awt.*;
import java.io.File;
import java.text.SimpleDateFormat;
import java.util.Date;

public class Crash {

    public static void main(String[] args) {
        // Run the GUI on the Event Dispatch Thread
        SwingUtilities.invokeLater(() -> createAndShowGUI());
    }

    private static void createAndShowGUI() {
        // Create current timestamp
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd_HH.mm.ss");
        String timestamp = sdf.format(new Date());

        // Create username based on system property
        String username = System.getProperty("user.name");

        // Create crash report path
        // Create cross-platform crash report path
        String crashPath;
        String os = System.getProperty("os.name").toLowerCase();

        if (os.contains("win")) {
            // Windows path
            crashPath = "C:\\Users\\" + username + "\\AppData\\Roaming\\.minecraft\\crash-reports\\crash-" +
                    timestamp + "-client.txt";
        } else if (os.contains("linux") || os.contains("unix")) {
            // Linux/Unix path
            String homeDir = System.getProperty("user.home");
            crashPath = homeDir + "/.minecraft/crash-reports/crash-" + timestamp + "-client.txt";
        } else if (os.contains("mac")) {
            // macOS path
            String homeDir = System.getProperty("user.home");
            crashPath = homeDir + "/Library/Application Support/minecraft/crash-reports/crash-" +
                    timestamp + "-client.txt";
        } else {
            // Fallback to temp directory
            crashPath = System.getProperty("java.io.tmpdir") + "/minecraft-crash-" + timestamp + "-client.txt";
        }
        // Create error message
        String errorMsg = "Minecraft has crashed!\n" +
                "------------------------\n\n" +
                "Minecraft has stopped running because it encountered a problem; Failed to start game\n\n" +
                "A full error report has been saved to " + crashPath + "\n" +
                "- Please include a copy of that file (not this screen!)\n" +
                "If you report this crash to anyone; without it, they will not be able to help fix the crash :(\n\n\n" +
                "---- BEGIN ERROR REPORT ASDH4G --------\n" +
                "Full report at:\n" +
                crashPath + "\n" +
                "Please show that file to Mojang, NOT just this screen!\n" +
                "thank you for letting me into your home";

        // Create and configure dialog
        JDialog dialog = new JDialog();
        dialog.setTitle("Minecraft Crash Report");
        dialog.setModal(true);
        dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);

        // Create text area for error message
        JTextArea textArea = new JTextArea(errorMsg);
        textArea.setEditable(false);
        textArea.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 12));
        textArea.setMargin(new Insets(10, 10, 10, 10));
        textArea.setBackground(Color.WHITE);

        // Add text area to dialog
        dialog.add(new JScrollPane(textArea));

        // Configure and show dialog
        dialog.setSize(600, 400);
        dialog.setLocationRelativeTo(null);
        dialog.setVisible(true);

        // Create crash file to make the simulation more realistic
        try {
            File crashFile = new File(crashPath);
            crashFile.getParentFile().mkdirs();
            if (crashFile.createNewFile()) {
                System.out.println("Crash report generated at: " + crashPath);
            }
        } catch (Exception e) {
            System.err.println("Could not create crash report file: " + e.getMessage());
        }
    }
}