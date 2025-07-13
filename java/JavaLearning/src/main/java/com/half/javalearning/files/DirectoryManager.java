package com.half.javalearning.files;


import java.io.IOException;
import java.nio.file.*;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

// Enhanced Directory Manager
public class DirectoryManager extends FileManager {

    public DirectoryManager(String dirPath) {
        super(dirPath);
    }

    @Override
    public boolean create() {
        try {
            Path dirPath = Paths.get(filePath);
            if (Files.exists(dirPath)) {
                if (Files.isDirectory(dirPath)) {
                    return true; // Already exists and is a directory
                } else {
                    System.err.println("Path exists but is not a directory: " + filePath);
                    return false;
                }
            }

            Files.createDirectories(dirPath);
            updateMetadata();
            System.out.println("Directory created: " + filePath);
            return true;
        } catch (IOException e) {
            System.err.println("Directory creation failed: " + filePath + " - " + e.getMessage());
            return false;
        }
    }

    public boolean createRecursive() {
        return create(); // Already recursive
    }

    public boolean isEmpty() {
        try (var stream = Files.list(Paths.get(filePath))) {
            return !stream.findFirst().isPresent();
        } catch (IOException e) {
            return false;
        }
    }

    public long getDirectorySize() {
        try (var stream = Files.walk(Paths.get(filePath))) {
            return stream
                    .filter(Files::isRegularFile)
                    .mapToLong(path -> {
                        try {
                            return Files.size(path);
                        } catch (IOException e) {
                            return 0L;
                        }
                    })
                    .sum();
        } catch (IOException e) {
            return 0L;
        }
    }

    public int getFileCount() {
        try (var stream = Files.walk(Paths.get(filePath))) {
            return (int) stream.filter(Files::isRegularFile).count();
        } catch (IOException e) {
            return 0;
        }
    }

    public int getDirectoryCount() {
        try (var stream = Files.walk(Paths.get(filePath))) {
            return (int) stream.filter(Files::isDirectory).count() - 1; // Exclude self
        } catch (IOException e) {
            return 0;
        }
    }

    public List<String> listRecursive() {
        try (var stream = Files.walk(Paths.get(filePath))) {
            return stream
                    .filter(path -> !path.equals(Paths.get(filePath)))
                    .map(Path::toString)
                    .sorted()
                    .collect(Collectors.toList());
        } catch (IOException e) {
            System.err.println("Recursive list failed: " + e.getMessage());
            return new ArrayList<>();
        }
    }

    public List<String> listFiles() {
        try (var stream = Files.list(Paths.get(filePath))) {
            return stream
                    .filter(Files::isRegularFile)
                    .map(Path::toString)
                    .sorted()
                    .collect(Collectors.toList());
        } catch (IOException e) {
            System.err.println("List files failed: " + e.getMessage());
            return new ArrayList<>();
        }
    }

    public List<String> listDirectories() {
        try (var stream = Files.list(Paths.get(filePath))) {
            return stream
                    .filter(Files::isDirectory)
                    .map(Path::toString)
                    .sorted()
                    .collect(Collectors.toList());
        } catch (IOException e) {
            System.err.println("List directories failed: " + e.getMessage());
            return new ArrayList<>();
        }
    }

    public List<String> listByExtension(String extension) {
        if (!extension.startsWith(".")) {
            extension = "." + extension;
        }
        final String ext = extension.toLowerCase();

        try (var stream = Files.walk(Paths.get(filePath))) {
            return stream
                    .filter(Files::isRegularFile)
                    .filter(path -> path.toString().toLowerCase().endsWith(ext))
                    .map(Path::toString)
                    .sorted()
                    .collect(Collectors.toList());
        } catch (IOException e) {
            System.err.println("List by extension failed: " + e.getMessage());
            return new ArrayList<>();
        }
    }

    public Map<String, Integer> getFileTypeStatistics() {
        Map<String, Integer> stats = new HashMap<>();
        try (var stream = Files.walk(Paths.get(filePath))) {
            stream.filter(Files::isRegularFile)
                    .forEach(path -> {
                        String fileName = path.getFileName().toString();
                        String ext = fileName.contains(".")
                                ? fileName.substring(fileName.lastIndexOf('.') + 1).toLowerCase()
                                : "no extension";
                        stats.merge(ext, 1, Integer::sum);
                    });
        } catch (IOException e) {
            System.err.println("File type statistics failed: " + e.getMessage());
        }
        return stats;
    }

    public boolean copyDirectory(String destination) {
        try {
            Path source = Paths.get(filePath);
            Path target = Paths.get(destination);

            Files.walk(source).forEach(sourcePath -> {
                try {
                    Path targetPath = target.resolve(source.relativize(sourcePath));
                    if (Files.isDirectory(sourcePath)) {
                        Files.createDirectories(targetPath);
                    } else {
                        Files.copy(sourcePath, targetPath, StandardCopyOption.REPLACE_EXISTING);
                    }
                } catch (IOException e) {
                    System.err.println("Failed to copy: " + sourcePath + " - " + e.getMessage());
                }
            });
            return true;
        } catch (IOException e) {
            System.err.println("Directory copy failed: " + e.getMessage());
            return false;
        }
    }

    public boolean moveDirectory(String destination) {
        try {
            Files.move(Paths.get(filePath), Paths.get(destination), StandardCopyOption.REPLACE_EXISTING);
            this.filePath = destination;
            return true;
        } catch (IOException e) {
            System.err.println("Directory move failed: " + e.getMessage());
            return false;
        }
    }

    public boolean deleteRecursive() {
        try {
            Files.walk(Paths.get(filePath))
                    .sorted(Comparator.reverseOrder())
                    .forEach(path -> {
                        try {
                            Files.delete(path);
                        } catch (IOException e) {
                            System.err.println("Failed to delete: " + path + " - " + e.getMessage());
                        }
                    });
            return true;
        } catch (IOException e) {
            System.err.println("Recursive delete failed: " + e.getMessage());
            return false;
        }
    }

    public boolean cleanDirectory() {
        try (var stream = Files.list(Paths.get(filePath))) {
            stream.forEach(path -> {
                try {
                    if (Files.isDirectory(path)) {
                        new DirectoryManager(path.toString()).deleteRecursive();
                    } else {
                        Files.delete(path);
                    }
                } catch (IOException e) {
                    System.err.println("Failed to clean: " + path + " - " + e.getMessage());
                }
            });
            return true;
        } catch (IOException e) {
            System.err.println("Directory clean failed: " + e.getMessage());
            return false;
        }
    }

    public void watchRecursive(FileWatcher watcher) {
        try {
            Path dir = Paths.get(filePath);
            WatchService watchService = FileSystems.getDefault().newWatchService();

            // Register the directory and all subdirectories
            Files.walk(dir)
                    .filter(Files::isDirectory)
                    .forEach(path -> {
                        try {
                            path.register(watchService,
                                    StandardWatchEventKinds.ENTRY_CREATE,
                                    StandardWatchEventKinds.ENTRY_DELETE,
                                    StandardWatchEventKinds.ENTRY_MODIFY);
                        } catch (IOException e) {
                            System.err.println("Failed to register watcher for: " + path);
                        }
                    });

            // Start watching
            CompletableFuture.runAsync(() -> {
                try {
                    WatchKey key;
                    while ((key = watchService.take()) != null) {
                        for (WatchEvent<?> event : key.pollEvents()) {
                            Path eventPath = ((Path) key.watchable()).resolve((Path) event.context());
                            String eventFile = eventPath.toString();

                            switch (event.kind().name()) {
                                case "ENTRY_CREATE" -> watcher.onCreated(eventFile);
                                case "ENTRY_DELETE" -> watcher.onDeleted(eventFile);
                                case "ENTRY_MODIFY" -> watcher.onModified(eventFile);
                            }
                        }
                        key.reset();
                    }
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            });

        } catch (IOException e) {
            System.err.println("Failed to set up recursive watcher: " + e.getMessage());
        }
    }

    @Override
    public String toString() {
        return String.format("DirectoryManager{path='%s', files=%d, dirs=%d, size=%s}",
                filePath, getFileCount(), getDirectoryCount(), formatFileSize(getDirectorySize()));
    }

    private String formatFileSize(long bytes) {
        if (bytes < 1024) return bytes + " B";
        int exp = (int) (Math.log(bytes) / Math.log(1024));
        String pre = "KMGTPE".charAt(exp - 1) + "";
        return String.format("%.1f %sB", bytes / Math.pow(1024, exp), pre);
    }
}
