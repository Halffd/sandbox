package com.half.javalearning.files;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.*;
import java.nio.file.attribute.*;
import java.security.DigestInputStream;
import java.security.MessageDigest;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.zip.*;

interface IFiles {
    // Core file operations
    String read();
    String read(String encoding);
    CompletableFuture<String> readAsync();

    void write(WriteMode mode);
    void write(String content, WriteMode mode);
    void append(String content);
    CompletableFuture<Void> writeAsync(String content, WriteMode mode);

    boolean delete();
    boolean exists();
    boolean rename(String newName);
    boolean copy(String destination);
    boolean move(String destination);

    List<String> list();
    List<Path> listPaths();
    List<String> search(String pattern);
    List<String> searchRegex(String regex);
    boolean create();
    boolean createDirectories();

    // Metadata operations
    void changePermissions(String permissions);
    void changeOwnership(String owner);
    Map<String, Object> getMetadata();
    String getChecksum();
    String getChecksum(String algorithm);

    // Compression with better options
    boolean compress(String outputPath);
    boolean compress(String outputPath, int compressionLevel);
    boolean decompress(String outputPath);
    List<String> listZipContents();

    // Security improvements
    boolean encrypt(String password, String algorithm);
    boolean decrypt(String password, String algorithm);

    // Utility improvements
    boolean backup(String backupPath, boolean timestamp);
    boolean restore(String backupPath);
    boolean sync(String targetPath, boolean checkContent);
    boolean convert(String targetFormat);

    // New utility methods
    void watch(FileWatcher watcher);
    boolean compare(String otherFilePath);
    long size();
    String getMimeType();

    enum WriteMode {
        FILE_WRITER("FileWriter - Simple text writing"),
        BUFFERED_WRITER("BufferedWriter - Optimized for large files"),
        PRINT_WRITER("PrintWriter - Formatted output with auto-flush"),
        FILE_OUTPUT_STREAM("FileOutputStream - Raw byte control"),
        ASYNC_WRITER("Asynchronous writing for non-blocking operations");

        private final String description;
        WriteMode(String description) { this.description = description; }
        public String getDescription() { return description; }
    }

    interface FileWatcher {
        void onModified(String filePath);
        void onDeleted(String filePath);
        void onCreated(String filePath);
    }
}

public class FileManager implements IFiles {
    private static final Map<String, WatchService> WATCH_SERVICES = new ConcurrentHashMap<>();
    private static final int DEFAULT_BUFFER_SIZE = 8192;

    // Core properties
    protected String fileName;
    protected String filePath;
    protected String fileContent;
    protected long fileSize;
    protected String fileExtension;
    protected String fileOwner;
    protected String filePermissions;
    protected LocalDateTime fileLastModified;
    protected LocalDateTime fileCreationDate;
    protected LocalDateTime fileAccessDate;
    protected String fileGroup;
    protected String mimeType;

    public FileManager(String filePath) {
        this.filePath = Objects.requireNonNull(filePath, "File path cannot be null");
        Path path = Paths.get(filePath);
        this.fileName = path.getFileName().toString();

        // Better extension extraction
        int lastDot = fileName.lastIndexOf('.');
        this.fileExtension = lastDot > 0 && lastDot < fileName.length() - 1
                ? fileName.substring(lastDot + 1).toLowerCase()
                : "";

        updateMetadata();
        detectMimeType();
    }

    protected void updateMetadata() {
        try {
            Path path = Paths.get(filePath);
            if (Files.exists(path)) {
                BasicFileAttributes attrs = Files.readAttributes(path, BasicFileAttributes.class);
                this.fileSize = attrs.size();
                this.fileLastModified = LocalDateTime.ofInstant(
                        attrs.lastModifiedTime().toInstant(),
                        java.time.ZoneId.systemDefault()
                );
                this.fileCreationDate = LocalDateTime.ofInstant(
                        attrs.creationTime().toInstant(),
                        java.time.ZoneId.systemDefault()
                );

                // POSIX attributes for Unix-like systems
                try {
                    PosixFileAttributes posixAttrs = Files.readAttributes(path, PosixFileAttributes.class);
                    this.fileOwner = posixAttrs.owner().getName();
                    this.filePermissions = PosixFilePermissions.toString(posixAttrs.permissions());
                    this.fileGroup = posixAttrs.group().getName();
                } catch (UnsupportedOperationException e) {
                    // Windows fallback
                    this.fileOwner = System.getProperty("user.name");
                    this.filePermissions = getWindowsPermissions(path);
                    this.fileGroup = "N/A";
                }
            }
        } catch (IOException e) {
            throw new RuntimeException("Failed to read file metadata: " + e.getMessage(), e);
        }
    }

    private String getWindowsPermissions(Path path) {
        StringBuilder perms = new StringBuilder();
        try {
            perms.append(Files.isReadable(path) ? "r" : "-");
            perms.append(Files.isWritable(path) ? "w" : "-");
            perms.append(Files.isExecutable(path) ? "x" : "-");
        } catch (Exception e) {
            return "unknown";
        }
        return perms.toString();
    }

    private void detectMimeType() {
        try {
            Path path = Paths.get(filePath);
            this.mimeType = Files.probeContentType(path);
            if (mimeType == null) {
                // Fallback based on extension
                this.mimeType = getMimeTypeByExtension(fileExtension);
            }
        } catch (IOException e) {
            this.mimeType = "application/octet-stream";
        }
    }

    private String getMimeTypeByExtension(String extension) {
        Map<String, String> mimeTypes = Map.of(
                "txt", "text/plain",
                "json", "application/json",
                "xml", "application/xml",
                "html", "text/html",
                "css", "text/css",
                "js", "application/javascript",
                "java", "text/x-java-source",
                "py", "text/x-python",
                "md", "text/markdown"
        );
        return mimeTypes.getOrDefault(extension.toLowerCase(), "application/octet-stream");
    }

    @Override
    public String toString() {
        return String.format("FileManager{path='%s', size=%d bytes, modified=%s, type=%s}",
                filePath, fileSize,
                fileLastModified != null ? fileLastModified.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME) : "unknown",
                mimeType);
    }

    // Core Operations Implementation
    @Override
    public String read() {
        return read(StandardCharsets.UTF_8.name());
    }

    @Override
    public String read(String encoding) {
        try {
            this.fileContent = Files.readString(Paths.get(filePath),
                    encoding != null ? java.nio.charset.Charset.forName(encoding) : StandardCharsets.UTF_8);
            this.fileAccessDate = LocalDateTime.now();
            return fileContent;
        } catch (IOException e) {
            throw new RuntimeException("Failed to read file: " + e.getMessage(), e);
        }
    }

    @Override
    public CompletableFuture<String> readAsync() {
        return CompletableFuture.supplyAsync(() -> read());
    }

    @Override
    public void write(WriteMode mode) {
        if (fileContent == null) {
            throw new IllegalStateException("No content to write! Use setFileContent() first.");
        }
        write(fileContent, mode);
    }

    @Override
    public void write(String content, WriteMode mode) {
        Objects.requireNonNull(content, "Content cannot be null");
        this.fileContent = content;

        try {
            // Ensure parent directories exist
            Path path = Paths.get(filePath);
            if (path.getParent() != null) {
                Files.createDirectories(path.getParent());
            }

            switch (mode) {
                case FILE_WRITER -> writeWithFileWriter(content);
                case BUFFERED_WRITER -> writeWithBufferedWriter(content);
                case PRINT_WRITER -> writeWithPrintWriter(content);
                case FILE_OUTPUT_STREAM -> writeWithFileOutputStream(content);
                case ASYNC_WRITER -> writeAsync(content, WriteMode.BUFFERED_WRITER);
                default -> throw new IllegalArgumentException("Unsupported write mode: " + mode);
            }
            updateMetadata();
        } catch (IOException e) {
            throw new RuntimeException("Write failed: " + e.getMessage(), e);
        }
    }

    @Override
    public void append(String content) {
        try {
            Files.writeString(Paths.get(filePath), content, StandardCharsets.UTF_8,
                    StandardOpenOption.CREATE, StandardOpenOption.APPEND);
            updateMetadata();
        } catch (IOException e) {
            throw new RuntimeException("Append failed: " + e.getMessage(), e);
        }
    }

    @Override
    public CompletableFuture<Void> writeAsync(String content, WriteMode mode) {
        return CompletableFuture.runAsync(() -> write(content, mode));
    }

    // Improved write methods with better resource management
    private void writeWithFileWriter(String content) throws IOException {
        try (FileWriter fw = new FileWriter(filePath, StandardCharsets.UTF_8)) {
            fw.write(content);
        }
    }

    private void writeWithBufferedWriter(String content) throws IOException {
        try (BufferedWriter bw = Files.newBufferedWriter(Paths.get(filePath), StandardCharsets.UTF_8)) {
            bw.write(content);
        }
    }

    private void writeWithPrintWriter(String content) throws IOException {
        try (PrintWriter pw = new PrintWriter(Files.newBufferedWriter(Paths.get(filePath), StandardCharsets.UTF_8))) {
            pw.print(content);
        }
    }

    private void writeWithFileOutputStream(String content) throws IOException {
        try (FileOutputStream fos = new FileOutputStream(filePath)) {
            fos.write(content.getBytes(StandardCharsets.UTF_8));
        }
    }

    @Override
    public boolean delete() {
        try {
            return Files.deleteIfExists(Paths.get(filePath));
        } catch (IOException e) {
            System.err.println("Delete failed: " + e.getMessage());
            return false;
        }
    }

    @Override
    public boolean exists() {
        return Files.exists(Paths.get(filePath));
    }

    @Override
    public boolean rename(String newName) {
        try {
            Path oldPath = Paths.get(filePath);
            Path newPath = oldPath.getParent().resolve(newName);
            Files.move(oldPath, newPath, StandardCopyOption.REPLACE_EXISTING);
            this.filePath = newPath.toString();
            this.fileName = newName;
            updateMetadata();
            return true;
        } catch (IOException e) {
            System.err.println("Rename failed: " + e.getMessage());
            return false;
        }
    }

    @Override
    public boolean copy(String destination) {
        try {
            Files.copy(Paths.get(filePath), Paths.get(destination),
                    StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.COPY_ATTRIBUTES);
            return true;
        } catch (IOException e) {
            System.err.println("Copy failed: " + e.getMessage());
            return false;
        }
    }

    @Override
    public boolean move(String destination) {
        try {
            Files.move(Paths.get(filePath), Paths.get(destination),
                    StandardCopyOption.REPLACE_EXISTING);
            this.filePath = destination;
            updateMetadata();
            return true;
        } catch (IOException e) {
            System.err.println("Move failed: " + e.getMessage());
            return false;
        }
    }

    @Override
    public List<String> list() {
        return listPaths().stream()
                .map(Path::toString)
                .collect(Collectors.toList());
    }

    @Override
    public List<Path> listPaths() {
        try {
            Path parentDir = Paths.get(filePath).getParent();
            if (parentDir == null) parentDir = Paths.get(".");

            try (var stream = Files.list(parentDir)) {
                return stream.sorted().collect(Collectors.toList());
            }
        } catch (IOException e) {
            System.err.println("List failed: " + e.getMessage());
            return new ArrayList<>();
        }
    }

    @Override
    public List<String> search(String pattern) {
        try {
            Path parentDir = Paths.get(filePath).getParent();
            if (parentDir == null) parentDir = Paths.get(".");

            try (var stream = Files.walk(parentDir)) {
                return stream
                        .filter(path -> path.toString().toLowerCase().contains(pattern.toLowerCase()))
                        .map(Path::toString)
                        .sorted()
                        .collect(Collectors.toList());
            }
        } catch (IOException e) {
            System.err.println("Search failed: " + e.getMessage());
            return new ArrayList<>();
        }
    }

    @Override
    public List<String> searchRegex(String regex) {
        Pattern pattern = Pattern.compile(regex, Pattern.CASE_INSENSITIVE);
        try {
            Path parentDir = Paths.get(filePath).getParent();
            if (parentDir == null) parentDir = Paths.get(".");

            try (var stream = Files.walk(parentDir)) {
                return stream
                        .filter(path -> pattern.matcher(path.toString()).find())
                        .map(Path::toString)
                        .sorted()
                        .collect(Collectors.toList());
            }
        } catch (IOException e) {
            System.err.println("Regex search failed: " + e.getMessage());
            return new ArrayList<>();
        }
    }

    @Override
    public boolean create() {
        try {
            Path path = Paths.get(filePath);
            if (path.getParent() != null) {
                Files.createDirectories(path.getParent());
            }
            Files.createFile(path);
            updateMetadata();
            return true;
        } catch (IOException e) {
            System.err.println("Create failed: " + e.getMessage());
            return false;
        }
    }

    @Override
    public boolean createDirectories() {
        try {
            Files.createDirectories(Paths.get(filePath).getParent());
            return true;
        } catch (IOException e) {
            System.err.println("Create directories failed: " + e.getMessage());
            return false;
        }
    }

    @Override
    public void changePermissions(String permissions) {
        try {
            Path path = Paths.get(filePath);
            Set<PosixFilePermission> perms = PosixFilePermissions.fromString(permissions);
            Files.setPosixFilePermissions(path, perms);
            this.filePermissions = permissions;
        } catch (Exception e) {
            System.err.println("Permission change failed (likely Windows): " + e.getMessage());
        }
    }

    @Override
    public void changeOwnership(String owner) {
        try {
            Path path = Paths.get(filePath);
            UserPrincipalLookupService lookupService = FileSystems.getDefault().getUserPrincipalLookupService();
            UserPrincipal userPrincipal = lookupService.lookupPrincipalByName(owner);
            Files.setOwner(path, userPrincipal);
            this.fileOwner = owner;
        } catch (Exception e) {
            System.err.println("Ownership change failed: " + e.getMessage());
        }
    }

    @Override
    public Map<String, Object> getMetadata() {
        Map<String, Object> metadata = new LinkedHashMap<>();
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");

        metadata.put("fileName", fileName);
        metadata.put("filePath", filePath);
        metadata.put("fileSize", fileSize);
        metadata.put("fileSizeHuman", formatFileSize(fileSize));
        metadata.put("fileExtension", fileExtension);
        metadata.put("mimeType", mimeType);
        metadata.put("fileOwner", fileOwner);
        metadata.put("filePermissions", filePermissions);
        metadata.put("fileGroup", fileGroup);
        metadata.put("exists", exists());
        metadata.put("readable", Files.isReadable(Paths.get(filePath)));
        metadata.put("writable", Files.isWritable(Paths.get(filePath)));
        metadata.put("executable", Files.isExecutable(Paths.get(filePath)));

        if (fileCreationDate != null) {
            metadata.put("creationDate", fileCreationDate.format(formatter));
        }
        if (fileLastModified != null) {
            metadata.put("lastModified", fileLastModified.format(formatter));
        }
        if (fileAccessDate != null) {
            metadata.put("lastAccessed", fileAccessDate.format(formatter));
        }

        return metadata;
    }

    private String formatFileSize(long bytes) {
        if (bytes < 1024) return bytes + " B";
        int exp = (int) (Math.log(bytes) / Math.log(1024));
        String pre = "KMGTPE".charAt(exp - 1) + "";
        return String.format("%.1f %sB", bytes / Math.pow(1024, exp), pre);
    }

    @Override
    public String getChecksum() {
        return getChecksum("SHA-256");
    }

    @Override
    public String getChecksum(String algorithm) {
        try {
            MessageDigest digest = MessageDigest.getInstance(algorithm);
            try (FileInputStream fis = new FileInputStream(filePath);
                 DigestInputStream dis = new DigestInputStream(fis, digest)) {

                byte[] buffer = new byte[DEFAULT_BUFFER_SIZE];
                while (dis.read(buffer) != -1) {
                    // Reading to compute digest
                }
            }

            byte[] hash = digest.digest();
            StringBuilder hexString = new StringBuilder();
            for (byte b : hash) {
                String hex = Integer.toHexString(0xff & b);
                if (hex.length() == 1) hexString.append('0');
                hexString.append(hex);
            }
            return hexString.toString();
        } catch (Exception e) {
            throw new RuntimeException("Checksum calculation failed: " + e.getMessage(), e);
        }
    }

    // Improved compression with better options
    @Override
    public boolean compress(String outputPath) {
        return compress(outputPath, Deflater.DEFAULT_COMPRESSION);
    }

    @Override
    public boolean compress(String outputPath, int compressionLevel) {
        try (FileOutputStream fos = new FileOutputStream(outputPath);
             ZipOutputStream zos = new ZipOutputStream(fos);
             FileInputStream fis = new FileInputStream(filePath)) {

            zos.setLevel(compressionLevel);
            ZipEntry zipEntry = new ZipEntry(fileName);
            zipEntry.setTime(System.currentTimeMillis());
            zos.putNextEntry(zipEntry);

            byte[] buffer = new byte[DEFAULT_BUFFER_SIZE];
            int length;
            while ((length = fis.read(buffer)) > 0) {
                zos.write(buffer, 0, length);
            }

            zos.closeEntry();
            return true;
        } catch (IOException e) {
            System.err.println("Compression failed: " + e.getMessage());
            return false;
        }
    }

    @Override
    public boolean decompress(String outputPath) {
        try (FileInputStream fis = new FileInputStream(filePath);
             ZipInputStream zis = new ZipInputStream(fis)) {

            ZipEntry zipEntry = zis.getNextEntry();
            if (zipEntry != null) {
                Path outputFile = Paths.get(outputPath);
                Files.createDirectories(outputFile.getParent());

                try (FileOutputStream fos = new FileOutputStream(outputPath)) {
                    byte[] buffer = new byte[DEFAULT_BUFFER_SIZE];
                    int length;
                    while ((length = zis.read(buffer)) > 0) {
                        fos.write(buffer, 0, length);
                    }
                }
                return true;
            }
            return false;
        } catch (IOException e) {
            System.err.println("Decompression failed: " + e.getMessage());
            return false;
        }
    }

    @Override
    public List<String> listZipContents() {
        List<String> contents = new ArrayList<>();
        try (FileInputStream fis = new FileInputStream(filePath);
             ZipInputStream zis = new ZipInputStream(fis)) {

            ZipEntry entry;
            while ((entry = zis.getNextEntry()) != null) {
                contents.add(entry.getName() + " (" + formatFileSize(entry.getSize()) + ")");
            }
        } catch (IOException e) {
            System.err.println("Failed to list zip contents: " + e.getMessage());
        }
        return contents;
    }

    // Enhanced security with proper algorithms
    @Override
    public boolean encrypt(String password, String algorithm) {
        // This is still basic XOR - in production, use proper encryption libraries
        // Like AES with proper key derivation (PBKDF2, scrypt, etc.)
        if (!"XOR".equalsIgnoreCase(algorithm)) {
            System.err.println("Only XOR encryption implemented in this demo. Use proper crypto libraries for production!");
            return false;
        }

        try {
            String content = read();
            if (content == null) return false;

            StringBuilder encrypted = new StringBuilder();
            byte[] passwordBytes = password.getBytes(StandardCharsets.UTF_8);
            byte[] contentBytes = content.getBytes(StandardCharsets.UTF_8);

            for (int i = 0; i < contentBytes.length; i++) {
                encrypted.append((char) (contentBytes[i] ^ passwordBytes[i % passwordBytes.length]));
            }

            this.fileContent = encrypted.toString();
            write(WriteMode.BUFFERED_WRITER);
            return true;
        } catch (Exception e) {
            System.err.println("Encryption failed: " + e.getMessage());
            return false;
        }
    }

    @Override
    public boolean decrypt(String password, String algorithm) {
        return encrypt(password, algorithm); // XOR is symmetric
    }

    @Override
    public boolean backup(String backupPath, boolean timestamp) {
        String finalBackupPath = backupPath;
        if (timestamp) {
            String timeStamp = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss"));
            finalBackupPath += "." + timeStamp + ".bak";
        }
        return copy(finalBackupPath);
    }

    @Override
    public boolean restore(String backupPath) {
        try {
            Files.copy(Paths.get(backupPath), Paths.get(filePath),
                    StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.COPY_ATTRIBUTES);
            updateMetadata();
            return true;
        } catch (IOException e) {
            System.err.println("Restore failed: " + e.getMessage());
            return false;
        }
    }

    @Override
    public boolean sync(String targetPath, boolean checkContent) {
        try {
            Path source = Paths.get(filePath);
            Path target = Paths.get(targetPath);

            if (!Files.exists(target)) {
                return copy(targetPath);
            }

            if (checkContent) {
                // Compare checksums for content verification
                FileManager targetFile = new FileManager(targetPath);
                return !getChecksum().equals(targetFile.getChecksum()) ? copy(targetPath) : true;
            } else {
                // Simple timestamp comparison
                return Files.getLastModifiedTime(source).compareTo(
                        Files.getLastModifiedTime(target)) > 0 ? copy(targetPath) : true;
            }
        } catch (IOException e) {
            System.err.println("Sync failed: " + e.getMessage());
            return false;
        }
    }

    @Override
    public boolean convert(String targetFormat) {
        if (!targetFormat.startsWith(".")) {
            targetFormat = "." + targetFormat;
        }

        String nameWithoutExt = fileName.contains(".")
                ? fileName.substring(0, fileName.lastIndexOf('.'))
                : fileName;
        String newFileName = nameWithoutExt + targetFormat;
        return rename(newFileName);
    }

    @Override
    public void watch(FileWatcher watcher) {
        try {
            Path dir = Paths.get(filePath).getParent();
            if (dir == null) dir = Paths.get(".");

            WatchService watchService = WATCH_SERVICES.computeIfAbsent(
                    dir.toString(),
                    k -> {
                        try {
                            return FileSystems.getDefault().newWatchService();
                        } catch (IOException e) {
                            throw new RuntimeException(e);
                        }
                    }
            );

            dir.register(watchService,
                    StandardWatchEventKinds.ENTRY_CREATE,
                    StandardWatchEventKinds.ENTRY_DELETE,
                    StandardWatchEventKinds.ENTRY_MODIFY);

            // Start watching in a separate thread
            CompletableFuture.runAsync(() -> {
                try {
                    WatchKey key;
                    while ((key = watchService.take()) != null) {
                        for (WatchEvent<?> event : key.pollEvents()) {
                            String eventFile = event.context().toString();
                            if (eventFile.equals(fileName)) {
                                switch (event.kind().name()) {
                                    case "ENTRY_CREATE" -> watcher.onCreated(filePath);
                                    case "ENTRY_DELETE" -> watcher.onDeleted(filePath);
                                    case "ENTRY_MODIFY" -> watcher.onModified(filePath);
                                }
                            }
                        }
                        key.reset();
                    }
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            });

        } catch (IOException e) {
            System.err.println("Failed to set up file watcher: " + e.getMessage());
        }
    }

    @Override
    public boolean compare(String otherFilePath) {
        try {
            FileManager other = new FileManager(otherFilePath);
            return getChecksum().equals(other.getChecksum());
        } catch (Exception e) {
            System.err.println("File comparison failed: " + e.getMessage());
            return false;
        }
    }

    @Override
    public long size() {
        return fileSize;
    }

    @Override
    public String getMimeType() {
        return mimeType;
    }

    // Getters and setters with validation
    public void setFileContent(String content) {
        this.fileContent = Objects.requireNonNull(content, "Content cannot be null");
    }

    public String getFileContent() {
        return fileContent;
    }

    public String getFileName() {
        return fileName;
    }

    public String getFilePath() {
        return filePath;
    }

    public long getFileSize() {
        return fileSize;
    }

    public String getFileExtension() {
        return fileExtension;
    }

    // Enhanced demo with comprehensive testing
    public static void main(String[] args) {
        System.out.println("=== Enhanced FileManager Demo ===\n");

        try {
            // Create test file
            FileManager fm = new FileManager("enhanced_demo.txt");
            String testContent = "Hello Enhanced FileManager! 🚀\n" +
                    "This demonstrates improved functionality:\n" +
                    "- Better error handling\n" +
                    "- Async operations\n" +
                    "- Content verification\n" +
                    "- File watching\n" +
                    "- Proper encoding support";

            // Test all write modes
            System.out.println("📝 Testing Write Modes:");
            for (WriteMode mode : WriteMode.values()) {
                if (mode != WriteMode.ASYNC_WRITER) { // Skip async for sync demo
                    System.out.println("  " + mode.getDescription());
                    fm.write(testContent + "\nWritten with: " + mode, mode);
                }
            }

            // Test async operations
            System.out.println("\n⚡ Testing Async Operations:");
            CompletableFuture<String> asyncRead = fm.readAsync();
            System.out.println("Async read completed: " + (asyncRead.get().length() > 0 ? "✅" : "❌"));

            // Test metadata
            System.out.println("\n📊 Enhanced Metadata:");
            fm.getMetadata().forEach((key, value) ->
                    System.out.println("  " + key + ": " + value));

            // Test checksum
            System.out.println("\n🔒 Security Features:");
            System.out.println("  SHA-256: " + fm.getChecksum());
            System.out.println("  MD5: " + fm.getChecksum("MD5"));

            // Test file operations
            System.out.println("\n📁 File Operations:");
            System.out.println("  Exists: " + fm.exists());
            System.out.println("  Size: " + fm.size() + " bytes");
            System.out.println("  MIME Type: " + fm.getMimeType());

            // Test compression with levels
            System.out.println("\n🗜️ Compression Tests:");
            boolean compressed = fm.compress("demo_compressed.zip", Deflater.BEST_COMPRESSION);
            System.out.println("  High compression: " + (compressed ? "✅" : "❌"));

            if (compressed) {
                FileManager zipFile = new FileManager("demo_compressed.zip");
                System.out.println("  Zip contents: " + zipFile.listZipContents());
            }

            // Test regex search
            System.out.println("\n🔍 Advanced Search:");
            List<String> regexResults = fm.searchRegex(".*\\.txt$");
            System.out.println("  Text files found: " + regexResults.size());

            // Test file watching
            System.out.println("\n👁️ File Watching:");
            fm.watch(new FileWatcher() {
                @Override
                public void onModified(String filePath) {
                    System.out.println("  📝 File modified: " + filePath);
                }

                @Override
                public void onDeleted(String filePath) {
                    System.out.println("  🗑️ File deleted: " + filePath);
                }

                @Override
                public void onCreated(String filePath) {
                    System.out.println("  ✨ File created: " + filePath);
                }
            });

            // Test file comparison
            FileManager copyFile = new FileManager("demo_copy.txt");
            copyFile.write(testContent, WriteMode.BUFFERED_WRITER);
            System.out.println("\n🔍 File Comparison:");
            System.out.println("  Files identical: " + fm.compare("demo_copy.txt"));

            // Test sync with content verification
            System.out.println("\n🔄 Smart Sync:");
            boolean synced = fm.sync("demo_sync.txt", true);
            System.out.println("  Content-based sync: " + (synced ? "✅" : "❌"));

            // Test directory operations
            System.out.println("\n📂 Directory Operations:");
            DirectoryManager dirManager = new DirectoryManager("test_directory");
            dirManager.create();
            System.out.println("  Directory created: " + dirManager.exists());
            System.out.println("  Directory size: " + dirManager.getDirectorySize() + " bytes");
            System.out.println("  File count: " + dirManager.getFileCount());

            // Test symlink operations
            System.out.println("\n🔗 Symlink Operations:");
            SymlinkManager symlinkManager = new SymlinkManager("test_symlink");
            boolean linkCreated = symlinkManager.createSymlink(fm.getFilePath());
            System.out.println("  Symlink created: " + (linkCreated ? "✅" : "❌"));
            if (linkCreated) {
                System.out.println("  Symlink target: " + symlinkManager.getTarget());
                System.out.println("  Is symlink: " + symlinkManager.isSymlink());
            }

            // Cleanup
            System.out.println("\n🧹 Cleanup:");
            fm.delete();
            copyFile.delete();
            new FileManager("demo_sync.txt").delete();
            new FileManager("demo_compressed.zip").delete();
            dirManager.delete();
            if (linkCreated) symlinkManager.delete();
            System.out.println("  Cleanup completed ✅");

        } catch (Exception e) {
            System.err.println("Demo failed: " + e.getMessage());
            e.printStackTrace();
        }
    }
}
