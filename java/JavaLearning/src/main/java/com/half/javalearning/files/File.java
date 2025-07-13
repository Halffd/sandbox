package com.half.javalearning.files;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.zip.Deflater;

// Enhanced File class with all the improvements
public class File extends FileManager {

    public File(String filePath) {
        super(filePath);
    }

    // Keep the intuitive toString behavior
    @Override
    public String toString() {
        String content = read();
        return content != null ? content : "";
    }

    // Enhanced assignment with encoding support
    public File set(String content) {
        return set(content, StandardCharsets.UTF_8.name());
    }

    public File set(String content, String encoding) {
        setFileContent(content);
        try {
            Path filePath = Paths.get(this.filePath);

            // Auto-create parent directories if they don't exist
            if (filePath.getParent() != null) {
                Files.createDirectories(filePath.getParent());
            }

            // Check if the target path already exists as a directory
            if (Files.exists(filePath) && Files.isDirectory(filePath)) {
                throw new IllegalArgumentException("Cannot write file - path is a directory: " + filePath);
            }

            Files.writeString(filePath, content,
                    java.nio.charset.Charset.forName(encoding));
            updateMetadata();
        } catch (IOException e) {
            throw new RuntimeException("Write failed: " + e.getMessage(), e);
        }
        return this;
    }

    // Enhanced concatenation with newline options
    public File concat(String additionalContent) {
        return concat(additionalContent, false);
    }

    public File concat(String additionalContent, boolean addNewline) {
        String currentContent = read();
        if (currentContent == null) currentContent = "";

        String separator = addNewline ? "\n" : "";
        setFileContent(currentContent + separator + additionalContent);
        write(WriteMode.BUFFERED_WRITER);
        return this;
    }

    // Fluent API methods
    public File add(String content) {
        return concat(content);
    }
    public File plus(String content) {
        super.append(content);
        return this;
    }

    public File newLine(String content) {
        return concat(content, true);
    }

    public File newLine() {
        return concat("", true);
    }

    public File clear() {
        set("");
        return this;
    }

    // Content analysis methods
    public boolean isEmpty() {
        String content = read();
        return content == null || content.trim().isEmpty();
    }

    public int length() {
        String content = read();
        return content != null ? content.length() : 0;
    }

    public int wordCount() {
        String content = read();
        if (content == null || content.trim().isEmpty()) return 0;
        return content.trim().split("\\s+").length;
    }

    public int lineCount() {
        String content = read();
        if (content == null || content.isEmpty()) return 0;
        return content.split("\n").length;
    }

    public String[] lines() {
        String content = read();
        return content != null ? content.split("\n") : new String[0];
    }

    public List<String> linesAsList() {
        return new ArrayList<>(Arrays.asList(lines())); // Make it mutable!
    }

    // Text processing methods
    public File replace(String target, String replacement) {
        String content = read();
        if (content != null) {
            set(content.replace(target, replacement));
        }
        return this;
    }

    public File replaceRegex(String regex, String replacement) {
        String content = read();
        if (content != null) {
            set(content.replaceAll(regex, replacement));
        }
        return this;
    }

    public File toUpperCase() {
        String content = read();
        if (content != null) {
            set(content.toUpperCase());
        }
        return this;
    }

    public File toLowerCase() {
        String content = read();
        if (content != null) {
            set(content.toLowerCase());
        }
        return this;
    }

    public File trim() {
        String content = read();
        if (content != null) {
            set(content.trim());
        }
        return this;
    }

    // Search methods
    public boolean contains(String text) {
        String content = read();
        return content != null && content.contains(text);
    }

    public boolean containsIgnoreCase(String text) {
        String content = read();
        return content != null && content.toLowerCase().contains(text.toLowerCase());
    }

    public boolean matches(String regex) {
        String content = read();
        return content != null && content.matches(regex);
    }

    public List<String> findMatches(String regex) {
        String content = read();
        if (content == null) return new ArrayList<>();

        Pattern pattern = Pattern.compile(regex);
        return pattern.matcher(content)
                .results()
                .map(result -> result.group())
                .collect(Collectors.toList());
    }

    public int count(String text) {
        String content = read();
        if (content == null || text.isEmpty()) return 0;

        int count = 0;
        int index = 0;
        while ((index = content.indexOf(text, index)) != -1) {
            count++;
            index += text.length();
        }
        return count;
    }

    // Line manipulation methods
    public File insertLineAt(int lineNumber, String text) {
        List<String> lines = linesAsList();
        if (lineNumber < 0 || lineNumber > lines.size()) {
            throw new IndexOutOfBoundsException("Line number out of range: " + lineNumber);
        }

        lines.add(lineNumber, text);
        set(String.join("\n", lines));
        return this;
    }

    public File removeLineAt(int lineNumber) {
        List<String> lines = linesAsList();
        if (lineNumber < 0 || lineNumber >= lines.size()) {
            throw new IndexOutOfBoundsException("Line number out of range: " + lineNumber);
        }

        lines.remove(lineNumber);
        set(String.join("\n", lines));
        return this;
    }

    public File replaceLineAt(int lineNumber, String newText) {
        List<String> lines = linesAsList();
        if (lineNumber < 0 || lineNumber >= lines.size()) {
            throw new IndexOutOfBoundsException("Line number out of range: " + lineNumber);
        }

        lines.set(lineNumber, newText);
        set(String.join("\n", lines));
        return this;
    }

    public String getLineAt(int lineNumber) {
        String[] lines = lines();
        if (lineNumber < 0 || lineNumber >= lines.length) {
            throw new IndexOutOfBoundsException("Line number out of range: " + lineNumber);
        }
        return lines[lineNumber];
    }

    // Content validation methods
    public boolean isValidJson() {
        try {
            String content = read();
            if (content == null) return false;
            // Simple JSON validation - would use Jackson or Gson in production
            content = content.trim();
            return (content.startsWith("{") && content.endsWith("}")) ||
                    (content.startsWith("[") && content.endsWith("]"));
        } catch (Exception e) {
            return false;
        }
    }

    public boolean isValidXml() {
        try {
            String content = read();
            if (content == null) return false;
            // Simple XML validation
            content = content.trim();
            return content.startsWith("<?xml") ||
                    (content.startsWith("<") && content.endsWith(">"));
        } catch (Exception e) {
            return false;
        }
    }

    // Statistics methods
    public Map<String, Object> getContentStatistics() {
        Map<String, Object> stats = new HashMap<>();
        String content = read();

        if (content == null) {
            stats.put("error", "Could not read file");
            return stats;
        }

        stats.put("characters", content.length());
        stats.put("charactersNoSpaces", content.replaceAll("\\s", "").length());
        stats.put("words", wordCount());
        stats.put("lines", lineCount());
        stats.put("paragraphs", content.split("\n\\s*\n").length);
        stats.put("sentences", content.split("[.!?]+").length);
        stats.put("averageWordsPerLine", wordCount() / (double) Math.max(1, lineCount()));

        // Character frequency
        Map<Character, Integer> charFreq = new HashMap<>();
        for (char c : content.toCharArray()) {
            charFreq.merge(c, 1, Integer::sum);
        }
        stats.put("mostCommonChar",
                charFreq.entrySet().stream()
                        .max(Map.Entry.comparingByValue())
                        .map(Map.Entry::getKey)
                        .orElse(' '));

        return stats;
    }

    // Enhanced demo showcasing all new features
    public static void main(String[] args) {
        System.out.println("=== Enhanced File Class Demo ===\n");

        try {
            // Create and populate test file
            File textFile = new File("enhanced_text_demo.txt");
            textFile.set("Welcome to the Enhanced File Manager!")
                    .newLine("This is line 2")
                    .newLine("This is line 3 with some UPPERCASE text")
                    .newLine("JSON example: {\"key\": \"value\"}")
                    .newLine("End of file");

            System.out.println("📝 Content Analysis:");
            System.out.println("  Content: " + textFile.toString().substring(0, 50) + "...");
            System.out.println("  Characters: " + textFile.length());
            System.out.println("  Words: " + textFile.wordCount());
            System.out.println("  Lines: " + textFile.lineCount());

            // Text processing
            System.out.println("\n🔄 Text Processing:");
            File processedFile = new File("processed_demo.txt");
            processedFile.set(textFile.toString())
                    .replace("Enhanced", "Super Enhanced")
                    .replaceRegex("line \\d+", "LINE X");
            System.out.println("  Processed content: " + processedFile.toString().substring(0, 60) + "...");

            // Line manipulation
            System.out.println("\n📏 Line Manipulation:");
            textFile.insertLineAt(1, ">>> INSERTED LINE <<<");
            System.out.println("  Line at index 1: " + textFile.getLineAt(1));

            textFile.replaceLineAt(2, ">>> REPLACED LINE <<<");
            System.out.println("  Replaced line 2: " + textFile.getLineAt(2));

            // Search operations
            System.out.println("\n🔍 Search Operations:");
            System.out.println("  Contains 'Enhanced': " + textFile.contains("Enhanced"));
            System.out.println("  Contains 'JSON' (ignore case): " + textFile.containsIgnoreCase("json"));
            System.out.println("  Count of 'line': " + textFile.count("line"));

            // Content validation
            System.out.println("\n✅ Content Validation:");
            File jsonFile = new File("test.json");
            jsonFile.set("{\"name\": \"test\", \"value\": 123}");
            System.out.println("  Is valid JSON: " + jsonFile.isValidJson());

            // Statistics
            System.out.println("\n📊 Content Statistics:");
            Map<String, Object> stats = textFile.getContentStatistics();
            stats.forEach((key, value) -> System.out.println("  " + key + ": " + value));

            // Directory operations
            System.out.println("\n📂 Directory Operations:");
            DirectoryManager testDir = new DirectoryManager("test_enhanced_dir");
            testDir.create();

            // Create some test files in the directory
            new File("test_enhanced_dir/file1.txt").set("Test file 1");
            new File("test_enhanced_dir/file2.md").set("# Test markdown");

            DirectoryManager subDir = new DirectoryManager("test_enhanced_dir/subdir");
            subDir.create();
            new File("test_enhanced_dir/subdir/nested.txt").set("Nested file content");

            System.out.println("  Directory created: " + testDir.exists());
            System.out.println("  Total files: " + testDir.getFileCount());
            System.out.println("  Total directories: " + testDir.getDirectoryCount());
            System.out.println("  Directory size: " + testDir.getDirectorySize() + " bytes");

            System.out.println("  Files by extension:");
            testDir.getFileTypeStatistics().forEach((ext, count) ->
                    System.out.println("    ." + ext + ": " + count + " files"));

            System.out.println("  Recursive listing:");
            testDir.listRecursive().forEach(path -> System.out.println("    " + path));

            // Symlink operations
            System.out.println("\n🔗 Symlink Operations:");
            SymlinkManager symlink = new SymlinkManager("test_symlink.txt");
            boolean linkCreated = symlink.createSymlink(textFile.getFilePath());

            if (linkCreated) {
                System.out.println("  Symlink created: ✅");
                System.out.println("  Is symlink: " + symlink.isSymlink());
                System.out.println("  Target: " + symlink.getTarget());
                System.out.println("  Absolute target: " + symlink.getAbsoluteTarget());
                System.out.println("  Target exists: " + symlink.isTargetExists());
                System.out.println("  Is broken: " + symlink.isBrokenLink());

                // Test symlink chain
                System.out.println("  Symlink chain:");
                symlink.getSymlinkChain().forEach((link, target) ->
                        System.out.println("    " + link + " -> " + target));

                // Test reading through symlink
                File symlinkFile = new File("test_symlink.txt");
                System.out.println("  Content via symlink: " + symlinkFile.toString().substring(0, 50) + "...");
            } else {
                System.out.println("  Symlink creation failed (possibly Windows without admin rights)");
            }

            // Advanced file operations
            System.out.println("\n🔧 Advanced Operations:");

            // File comparison
            File duplicate = new File("duplicate.txt");
            duplicate.set(textFile.toString());
            System.out.println("  Files identical: " + textFile.compare(duplicate.getFilePath()));

            // Checksum verification
            System.out.println("  Original checksum: " + textFile.getChecksum().substring(0, 16) + "...");
            System.out.println("  Duplicate checksum: " + duplicate.getChecksum().substring(0, 16) + "...");

            // Backup with timestamp
            boolean backedUp = textFile.backup("backups/", true);
            System.out.println("  Backup created: " + (backedUp ? "✅" : "❌"));

            // Compression test
            boolean compressed = textFile.compress("text_file.zip", Deflater.BEST_COMPRESSION);
            if (compressed) {
                FileManager zipFile = new FileManager("text_file.zip");
                System.out.println("  Compressed size: " + zipFile.getFileSize() + " bytes");
                System.out.println("  Compression ratio: " +
                        String.format("%.1f%%", (1.0 - (double)zipFile.getFileSize() / textFile.getFileSize()) * 100));
                System.out.println("  Zip contents: " + zipFile.listZipContents());
            }

            // File watching demo
            System.out.println("\n👁️ File Watching Demo:");
            textFile.watch(new IFiles.FileWatcher() {
                @Override
                public void onModified(String filePath) {
                    System.out.println("  🔄 File modified: " + Paths.get(filePath).getFileName());
                }

                @Override
                public void onDeleted(String filePath) {
                    System.out.println("  🗑️ File deleted: " + Paths.get(filePath).getFileName());
                }

                @Override
                public void onCreated(String filePath) {
                    System.out.println("  ✨ File created: " + Paths.get(filePath).getFileName());
                }
            });

            // Trigger file modification to test watcher
            Thread.sleep(100); // Give watcher time to set up
            textFile.plus("\nThis line was added to trigger the file watcher!");
            Thread.sleep(100); // Give watcher time to detect change

            // Directory watching
            System.out.println("\n👁️ Directory Watching Demo:");
            testDir.watchRecursive(new IFiles.FileWatcher() {
                @Override
                public void onModified(String filePath) {
                    System.out.println("  📁 Directory change - Modified: " + Paths.get(filePath).getFileName());
                }

                @Override
                public void onDeleted(String filePath) {
                    System.out.println("  📁 Directory change - Deleted: " + Paths.get(filePath).getFileName());
                }

                @Override
                public void onCreated(String filePath) {
                    System.out.println("  📁 Directory change - Created: " + Paths.get(filePath).getFileName());
                }
            });

            // Create a new file to trigger directory watcher
            Thread.sleep(100);
            new File("test_enhanced_dir/new_watched_file.txt").set("This file creation should be detected!");
            Thread.sleep(100);

            // Async operations demo
            System.out.println("\n⚡ Asynchronous Operations:");
            CompletableFuture<String> asyncRead = textFile.readAsync();
            CompletableFuture<Void> asyncWrite = new File("async_test.txt")
                    .writeAsync("Async content!", IFiles.WriteMode.BUFFERED_WRITER);

            CompletableFuture.allOf(asyncRead, asyncWrite).get(); // Wait for completion
            System.out.println("  Async read length: " + asyncRead.get().length());
            System.out.println("  Async file exists: " + new File("async_test.txt").exists());

            // Performance comparison
            System.out.println("\n⚡ Performance Comparison:");
            String largeContent = "Large content test\n".repeat(10000);

            long startTime = System.nanoTime();
            new File("perf_test_buffered.txt").set(largeContent);
            long bufferedTime = System.nanoTime() - startTime;

            startTime = System.nanoTime();
            FileManager perfTest = new FileManager("perf_test_regular.txt");
            perfTest.setFileContent(largeContent);
            perfTest.write(IFiles.WriteMode.FILE_WRITER);
            long regularTime = System.nanoTime() - startTime;

            System.out.printf("  Buffered write: %.2f ms%n", bufferedTime / 1_000_000.0);
            System.out.printf("  Regular write: %.2f ms%n", regularTime / 1_000_000.0);
            System.out.printf("  Speedup: %.1fx%n", (double)regularTime / bufferedTime);

            // Metadata comparison
            System.out.println("\n📋 Enhanced Metadata:");
            Map<String, Object> metadata = textFile.getMetadata();
            metadata.forEach((key, value) -> System.out.println("  " + key + ": " + value));

            // Cleanup with progress
            System.out.println("\n🧹 Comprehensive Cleanup:");
            List<String> filesToClean = Arrays.asList(
                    "enhanced_text_demo.txt", "processed_demo.txt", "test.json",
                    "duplicate.txt", "text_file.zip", "async_test.txt",
                    "perf_test_buffered.txt", "perf_test_regular.txt"
            );

            filesToClean.forEach(fileName -> {
                boolean deleted = new File(fileName).delete();
                System.out.println("  " + fileName + ": " + (deleted ? "✅" : "❌"));
            });

            if (linkCreated) {
                boolean symlinkDeleted = symlink.delete();
                System.out.println("  test_symlink.txt: " + (symlinkDeleted ? "✅" : "❌"));
            }

            boolean dirDeleted = testDir.deleteRecursive();
            System.out.println("  test_enhanced_dir/: " + (dirDeleted ? "✅" : "❌"));

            // Clean up backup directory if it exists
            DirectoryManager backupDir = new DirectoryManager("backups");
            if (backupDir.exists()) {
                boolean backupCleaned = backupDir.deleteRecursive();
                System.out.println("  backups/: " + (backupCleaned ? "✅" : "❌"));
            }

            System.out.println("\n🎉 Demo completed successfully!");
            System.out.println("Features demonstrated:");
            System.out.println("  ✅ Enhanced file operations with fluent API");
            System.out.println("  ✅ Directory management with recursive operations");
            System.out.println("  ✅ Symlink creation and management");
            System.out.println("  ✅ Content analysis and text processing");
            System.out.println("  ✅ Asynchronous file operations");
            System.out.println("  ✅ File watching and monitoring");
            System.out.println("  ✅ Advanced search and filtering");
            System.out.println("  ✅ Compression and security features");
            System.out.println("  ✅ Performance optimizations");
            System.out.println("  ✅ Comprehensive metadata handling");

        } catch (Exception e) {
            System.err.println("Demo failed: " + e.getMessage());
            e.printStackTrace();
        }
    }
}