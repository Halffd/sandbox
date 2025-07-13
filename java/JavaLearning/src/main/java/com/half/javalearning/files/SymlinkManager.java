package com.half.javalearning.files;


import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.stream.Collectors;

// Symlink Manager
public class SymlinkManager extends FileManager {

    public SymlinkManager(String linkPath) {
        super(linkPath);
    }

    public boolean createSymlink(String targetPath) {
        try {
            Path link = Paths.get(filePath);
            Path target = Paths.get(targetPath);

            // Ensure target exists
            if (!Files.exists(target)) {
                System.err.println("Target does not exist: " + targetPath);
                return false;
            }

            // Create parent directories if needed
            if (link.getParent() != null) {
                Files.createDirectories(link.getParent());
            }

            Files.createSymbolicLink(link, target);
            updateMetadata();
            return true;
        } catch (IOException | UnsupportedOperationException e) {
            System.err.println("Symlink creation failed: " + e.getMessage());
            return false;
        }
    }

    public boolean createHardLink(String targetPath) {
        try {
            Path link = Paths.get(filePath);
            Path target = Paths.get(targetPath);

            if (!Files.exists(target)) {
                System.err.println("Target does not exist: " + targetPath);
                return false;
            }

            if (link.getParent() != null) {
                Files.createDirectories(link.getParent());
            }

            Files.createLink(link, target);
            updateMetadata();
            return true;
        } catch (IOException | UnsupportedOperationException e) {
            System.err.println("Hard link creation failed: " + e.getMessage());
            return false;
        }
    }

    public boolean isSymlink() {
        return Files.isSymbolicLink(Paths.get(filePath));
    }

    public String getTarget() {
        try {
            if (isSymlink()) {
                return Files.readSymbolicLink(Paths.get(filePath)).toString();
            }
            return null;
        } catch (IOException e) {
            System.err.println("Failed to read symlink target: " + e.getMessage());
            return null;
        }
    }

    public String getAbsoluteTarget() {
        try {
            if (!isSymlink()) {
                return null;
            }

            Path symlinkPath = Paths.get(filePath).toAbsolutePath(); // Make symlink path absolute first
            Path target = Files.readSymbolicLink(symlinkPath);

            if (target.isAbsolute()) {
                return target.toString();
            } else {
                // Resolve relative target against the symlink's parent directory
                Path parent = symlinkPath.getParent();
                if (parent == null) {
                    // This should never happen with absolute paths, but just in case
                    parent = Paths.get(System.getProperty("user.dir"));
                }
                return parent.resolve(target).normalize().toString();
            }
        } catch (IOException e) {
            System.err.println("Failed to resolve symlink target: " + e.getMessage());
            return null;
        }
    }
    public boolean isTargetExists() {
        String target = getAbsoluteTarget();
        return target != null && Files.exists(Paths.get(target));
    }
    public boolean isBrokenLink() {
        return isSymlink() && !isTargetExists();
    }

    public boolean repairLink(String newTargetPath) {
        if (!isSymlink()) {
            System.err.println("Not a symbolic link: " + filePath);
            return false;
        }

        try {
            Files.delete(Paths.get(filePath));
            return createSymlink(newTargetPath);
        } catch (IOException e) {
            System.err.println("Failed to repair symlink: " + e.getMessage());
            return false;
        }
    }

    public List<String> findAllSymlinks(String searchPath) {
        try (var stream = Files.walk(Paths.get(searchPath))) {
            return stream
                    .filter(Files::isSymbolicLink)
                    .map(Path::toString)
                    .sorted()
                    .collect(Collectors.toList());
        } catch (IOException e) {
            System.err.println("Failed to find symlinks: " + e.getMessage());
            return new ArrayList<>();
        }
    }

    public List<String> findBrokenSymlinks(String searchPath) {
        List<String> brokenLinks = new ArrayList<>();
        try (var stream = Files.walk(Paths.get(searchPath))) {
            stream.filter(Files::isSymbolicLink)
                    .forEach(link -> {
                        try {
                            Path target = Files.readSymbolicLink(link);
                            Path absoluteTarget = link.getParent().resolve(target).normalize();
                            if (!Files.exists(absoluteTarget)) {
                                brokenLinks.add(link.toString());
                            }
                        } catch (IOException e) {
                            brokenLinks.add(link.toString());
                        }
                    });
        } catch (IOException e) {
            System.err.println("Failed to find broken symlinks: " + e.getMessage());
        }
        return brokenLinks;
    }

    public Map<String, String> getSymlinkChain() {
        Map<String, String> chain = new LinkedHashMap<>();
        String current = Paths.get(filePath).toAbsolutePath().toString(); // Start with absolute path
        Set<String> visited = new HashSet<>();

        while (Files.isSymbolicLink(Paths.get(current)) && !visited.contains(current)) {
            visited.add(current);
            try {
                Path currentPath = Paths.get(current);
                String target = Files.readSymbolicLink(currentPath).toString();
                chain.put(current, target);

                // Resolve relative paths properly
                if (!Paths.get(target).isAbsolute()) {
                    Path parent = currentPath.getParent();
                    if (parent == null) {
                        parent = Paths.get(System.getProperty("user.dir"));
                    }
                    current = parent.resolve(target).normalize().toString();
                } else {
                    current = target;
                }
            } catch (IOException e) {
                chain.put(current, "ERROR: " + e.getMessage());
                break;
            }
        }

        // Add final target
        if (!Files.isSymbolicLink(Paths.get(current))) {
            chain.put(current, Files.exists(Paths.get(current)) ? "FILE" : "NOT_FOUND");
        } else if (visited.contains(current)) {
            chain.put(current, "CIRCULAR_REFERENCE");
        }

        return chain;
    }

    public boolean hasCircularReference() {
        return getSymlinkChain().containsValue("CIRCULAR_REFERENCE");
    }

    @Override
    public String toString() {
        if (isSymlink()) {
            String target = getTarget();
            boolean exists = isTargetExists();
            return String.format("SymlinkManager{link='%s' -> '%s', exists=%s, broken=%s}",
                    filePath, target, exists, !exists);
        } else {
            return String.format("SymlinkManager{path='%s', not_a_symlink=true}", filePath);
        }
    }
}
