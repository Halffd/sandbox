package com.half.javalearning;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.OutputStream;
import java.io.PrintStream;

/**
 * @author half
 */
public class Announcement {

    public static void announce(String[] args) {
        testAnnouncement(args);
    }

    public static void testAnnouncement(String[] args) {
        // Print a greeting message
        System.out.println(AnsiColors.GREEN + "Hello World!" + AnsiColors.RESET);

        AnnouncementPrinter printer = new AnnouncementPrinter();
        try {
            printer.makeAnnouncement();
        } catch (FileNotFoundException e) {
            System.err.println(AnsiColors.RED + "Error: " + e.getMessage() + AnsiColors.RESET);
        }

        // Using Printer from Streams class
        try {
            Printer streamPrinter = new Printer(getClassName());
            streamPrinter.print();
        } catch (Exception error) {
            System.err.println(AnsiColors.RED + "Error: " + error.getMessage() + AnsiColors.RESET);
        }
    }

    public static String getClassName() {
        return "JavaLearning";
    }
}

class AnnouncementPrinter {

    void printToDifferentStreams(PrintStream out, String message) {
        out.println(message);
    }

    void makeAnnouncement() throws FileNotFoundException {
        String announcement = "Important announcement ";

        // Print to standard output and error streams with colors
        printToDifferentStreams(System.out, AnsiColors.CYAN + announcement + Numbers.ONE + AnsiColors.RESET);
        printToDifferentStreams(System.err, AnsiColors.YELLOW + "Warning: " + announcement + AnsiColors.RESET);

        // Create a platform-independent file path
        String filePath = "announcement.txt"; // Use relative path for multi-platform compatibility
        PrintStream fileStream = new PrintStream(new File(filePath));
        printToDifferentStreams(fileStream, announcement);

        // Show the absolute path of the stored file
        File file = new File(filePath);
        System.out.println(AnsiColors.GREEN + "File stored at: " + file.getAbsolutePath() + AnsiColors.RESET);
    }
}

// Class to hold ANSI color codes
class AnsiColors {
    public static final String RESET = "\033[0m"; // Reset to default color
    public static final String GREEN = "\033[32m"; // Green for normal output
    public static final String RED = "\033[31m";   // Red for error messages
    public static final String YELLOW = "\033[33m"; // Yellow for warnings
    public static final String CYAN = "\033[36m";   // Cyan for announcements
}

class Numbers {
    public static final int ZERO = 0;
    public static final int ONE = 1;
    public static final int FORTY_TWO = 42;
    public static final int DEFAULT = -1;
}

class Printer {
    private String toBePrinted;
    private final OutputStream outputStream;

    public Printer() {
        this.outputStream = System.out;
        this.toBePrinted = "Hello World";
    }

    public Printer(String toBePrinted) {
        this.outputStream = System.out;
        setToBePrinted(toBePrinted);
    }

    private void setToBePrinted(String whatWillWePrint) {
        this.toBePrinted = whatWillWePrint;
    }

    public void print() {
        preparatoryPrint(new PrintStream(outputStream));
    }

    private void preparatoryPrint(PrintStream out) {
        out.print(this);
        printDetails(out);
    }

    private void printDetails(PrintStream out) {
        out.print(toBePrinted);
    }

    @Override
    public String toString() {
        return "Print: " + toBePrinted;
    }
}
