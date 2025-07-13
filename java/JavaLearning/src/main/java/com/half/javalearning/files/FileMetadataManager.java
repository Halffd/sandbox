package com.half.javalearning.files;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Professional file and image management system with metadata support.
 * Provides comprehensive functionality for storing, retrieving, and managing
 * files and images with associated metadata.
 *
 * @author Professional Development Team
 * @version 2.0
 * @since 2025-01-27
 */
public class FileMetadataManager {

    private static final Logger LOGGER = Logger.getLogger(FileMetadataManager.class.getName());

    // Protocol constants
    private static final String METADATA_SECTION = "===METADATA===";
    private static final String CONTENT_SECTION = "===CONTENT===";
    private static final String END_SECTION = "===END===";
    private static final String METADATA_EXTENSION = ".metadata";
    private static final String DEFAULT_IMAGE_FORMAT = "PNG";

    // Supported image formats
    private static final Set<String> SUPPORTED_IMAGE_FORMATS =
            Set.of("PNG", "JPG", "JPEG", "GIF", "BMP", "TIFF");

    /**
     * Creates a text file with associated metadata.
     *
     * @param filePath The path where the file will be created
     * @param content The text content to store
     * @param metadata Key-value pairs of metadata
     * @throws FileOperationException if file creation fails
     */
    public void createTextFile(String filePath, String content, Map<String, String> metadata)
            throws FileOperationException, IOException {

        validateFilePath(filePath);
        Objects.requireNonNull(content, "Content cannot be null");
        Objects.requireNonNull(metadata, "Metadata cannot be null");

        Path path = Paths.get(filePath);
        createDirectoryIfNotExists(path.getParent());

        try (BufferedWriter writer = Files.newBufferedWriter(path, StandardCharsets.UTF_8)) {
            writeMetadataSection(writer, metadata);
            writeContentSection(writer, content);

            LOGGER.info("Successfully created text file: " + filePath);

        } catch (IOException e) {
            String errorMsg = "Failed to create text file: " + filePath;
            LOGGER.log(Level.SEVERE, errorMsg, e);
            throw new FileOperationException(errorMsg, e);
        }
    }

    /**
     * Creates an image file with associated metadata stored separately.
     *
     * @param filePath The path where the image will be saved
     * @param image The BufferedImage to store
     * @param metadata Key-value pairs of metadata
     * @param format The image format (PNG, JPG, etc.)
     * @throws FileOperationException if image creation fails
     */
    public void createImageFile(String filePath, BufferedImage image,
                                Map<String, String> metadata, String format)
            throws FileOperationException, IOException {

        validateFilePath(filePath);
        Objects.requireNonNull(image, "Image cannot be null");
        Objects.requireNonNull(metadata, "Metadata cannot be null");

        String imageFormat = format != null ? format.toUpperCase() : DEFAULT_IMAGE_FORMAT;
        if (!SUPPORTED_IMAGE_FORMATS.contains(imageFormat)) {
            throw new IllegalArgumentException("Unsupported image format: " + imageFormat);
        }

        Path imagePath = Paths.get(filePath);
        Path metadataPath = Paths.get(filePath + METADATA_EXTENSION);

        createDirectoryIfNotExists(imagePath.getParent());

        try {
            // Save the image
            ImageIO.write(image, imageFormat, imagePath.toFile());

            // Save metadata with image reference
            Map<String, String> enhancedMetadata = new HashMap<>(metadata);
            enhancedMetadata.put("image_file", imagePath.getFileName().toString());
            enhancedMetadata.put("image_format", imageFormat);
            enhancedMetadata.put("created_timestamp", getCurrentTimestamp());

            createTextFile(metadataPath.toString(), "", enhancedMetadata);

            LOGGER.info("Successfully created image file: " + filePath);

        } catch (IOException e) {
            String errorMsg = "Failed to create image file: " + filePath;
            LOGGER.log(Level.SEVERE, errorMsg, e);
            throw new FileOperationException(errorMsg, e);
        }
    }

    /**
     * Retrieves a text file with its metadata.
     *
     * @param filePath The path to the file
     * @return FileContent object containing content and metadata
     * @throws FileOperationException if file retrieval fails
     */
    public FileContent retrieveTextFile(String filePath) throws FileOperationException {
        validateFilePath(filePath);

        Path path = Paths.get(filePath);
        if (!Files.exists(path)) {
            throw new FileOperationException("File does not exist: " + filePath);
        }

        try (BufferedReader reader = Files.newBufferedReader(path, StandardCharsets.UTF_8)) {
            Map<String, String> metadata = new HashMap<>();
            StringBuilder content = new StringBuilder();

            ParseState state = ParseState.SEEKING_METADATA;
            String line;

            while ((line = reader.readLine()) != null) {
                state = processLine(line, state, metadata, content);
                if (state == ParseState.COMPLETED) {
                    break;
                }
            }

            LOGGER.info("Successfully retrieved text file: " + filePath);
            return new FileContent(content.toString().trim(), metadata);

        } catch (IOException e) {
            String errorMsg = "Failed to retrieve text file: " + filePath;
            LOGGER.log(Level.SEVERE, errorMsg, e);
            throw new FileOperationException(errorMsg, e);
        }
    }

    /**
     * Retrieves an image file with its metadata.
     *
     * @param filePath The path to the image file
     * @return ImageContent object containing image and metadata
     * @throws FileOperationException if image retrieval fails
     */
    public ImageContent retrieveImageFile(String filePath) throws FileOperationException {
        validateFilePath(filePath);

        Path imagePath = Paths.get(filePath);
        Path metadataPath = Paths.get(filePath + METADATA_EXTENSION);

        if (!Files.exists(imagePath)) {
            throw new FileOperationException("Image file does not exist: " + filePath);
        }

        try {
            BufferedImage image = ImageIO.read(imagePath.toFile());
            if (image == null) {
                throw new FileOperationException("Unable to read image file: " + filePath);
            }

            Map<String, String> metadata = new HashMap<>();
            if (Files.exists(metadataPath)) {
                FileContent metadataContent = retrieveTextFile(metadataPath.toString());
                metadata = metadataContent.getMetadata();
            }

            LOGGER.info("Successfully retrieved image file: " + filePath);
            return new ImageContent(image, metadata);

        } catch (IOException e) {
            String errorMsg = "Failed to retrieve image file: " + filePath;
            LOGGER.log(Level.SEVERE, errorMsg, e);
            throw new FileOperationException(errorMsg, e);
        }
    }

    /**
     * Lists all files in a directory with their metadata.
     *
     * @param directoryPath The directory to scan
     * @return List of file information objects
     * @throws FileOperationException if directory access fails
     */
    public List<FileInfo> listFiles(String directoryPath) throws FileOperationException {
        validateFilePath(directoryPath);

        Path directory = Paths.get(directoryPath);
        if (!Files.exists(directory) || !Files.isDirectory(directory)) {
            throw new FileOperationException("Directory does not exist: " + directoryPath);
        }

        List<FileInfo> fileInfoList = new ArrayList<>();

        try {
            Files.list(directory)
                    .filter(path -> !path.toString().endsWith(METADATA_EXTENSION))
                    .forEach(path -> {
                        try {
                            FileInfo info = createFileInfo(path);
                            fileInfoList.add(info);
                        } catch (Exception e) {
                            LOGGER.log(Level.WARNING, "Failed to process file: " + path, e);
                        }
                    });

            LOGGER.info("Successfully listed files in directory: " + directoryPath);
            return fileInfoList;

        } catch (IOException e) {
            String errorMsg = "Failed to list files in directory: " + directoryPath;
            LOGGER.log(Level.SEVERE, errorMsg, e);
            throw new FileOperationException(errorMsg, e);
        }
    }

    // Private helper methods

    private void validateFilePath(String filePath) {
        if (filePath == null || filePath.trim().isEmpty()) {
            throw new IllegalArgumentException("File path cannot be null or empty");
        }
    }

    private void createDirectoryIfNotExists(Path directory) throws IOException {
        if (directory != null && !Files.exists(directory)) {
            Files.createDirectories(directory);
        }
    }

    private void writeMetadataSection(BufferedWriter writer, Map<String, String> metadata)
            throws IOException {
        writer.write(METADATA_SECTION + "\n");
        for (Map.Entry<String, String> entry : metadata.entrySet()) {
            writer.write(entry.getKey() + "=" + entry.getValue() + "\n");
        }
    }

    private void writeContentSection(BufferedWriter writer, String content) throws IOException {
        writer.write(CONTENT_SECTION + "\n");
        writer.write(content);
        writer.write("\n" + END_SECTION + "\n");
    }

    private ParseState processLine(String line, ParseState currentState,
                                   Map<String, String> metadata, StringBuilder content) {
        switch (currentState) {
            case SEEKING_METADATA:
                if (line.equals(METADATA_SECTION)) {
                    return ParseState.READING_METADATA;
                }
                break;
            case READING_METADATA:
                if (line.equals(CONTENT_SECTION)) {
                    return ParseState.READING_CONTENT;
                } else if (line.contains("=")) {
                    String[] parts = line.split("=", 2);
                    metadata.put(parts[0].trim(), parts[1].trim());
                }
                break;
            case READING_CONTENT:
                if (line.equals(END_SECTION)) {
                    return ParseState.COMPLETED;
                } else {
                    content.append(line).append("\n");
                }
                break;
        }
        return currentState;
    }

    private String getCurrentTimestamp() {
        return LocalDateTime.now().format(DateTimeFormatter.ISO_LOCAL_DATE_TIME);
    }

    private FileInfo createFileInfo(Path path) throws IOException {
        String fileName = path.getFileName().toString();
        long fileSize = Files.size(path);
        String lastModified = Files.getLastModifiedTime(path).toString();

        Map<String, String> metadata = new HashMap<>();
        Path metadataPath = Paths.get(path.toString() + METADATA_EXTENSION);

        if (Files.exists(metadataPath)) {
            try {
                FileContent metadataContent = retrieveTextFile(metadataPath.toString());
                metadata = metadataContent.getMetadata();
            } catch (FileOperationException e) {
                LOGGER.log(Level.WARNING, "Failed to read metadata for: " + fileName, e);
            }
        }

        return new FileInfo(fileName, fileSize, lastModified, metadata);
    }

    // Enums and Data Classes

    private enum ParseState {
        SEEKING_METADATA, READING_METADATA, READING_CONTENT, COMPLETED
    }

    /**
     * Represents file content with metadata.
     */
    public static class FileContent {
        private final String content;
        private final Map<String, String> metadata;

        public FileContent(String content, Map<String, String> metadata) {
            this.content = content;
            this.metadata = Collections.unmodifiableMap(metadata);
        }

        public String getContent() { return content; }
        public Map<String, String> getMetadata() { return metadata; }
    }

    /**
     * Represents image content with metadata.
     */
    public static class ImageContent {
        private final BufferedImage image;
        private final Map<String, String> metadata;

        public ImageContent(BufferedImage image, Map<String, String> metadata) {
            this.image = image;
            this.metadata = Collections.unmodifiableMap(metadata);
        }

        public BufferedImage getImage() { return image; }
        public Map<String, String> getMetadata() { return metadata; }
    }

    /**
     * Represents file information with metadata.
     */
    public static class FileInfo {
        private final String fileName;
        private final long fileSize;
        private final String lastModified;
        private final Map<String, String> metadata;

        public FileInfo(String fileName, long fileSize, String lastModified,
                        Map<String, String> metadata) {
            this.fileName = fileName;
            this.fileSize = fileSize;
            this.lastModified = lastModified;
            this.metadata = Collections.unmodifiableMap(metadata);
        }

        public String getFileName() { return fileName; }
        public long getFileSize() { return fileSize; }
        public String getLastModified() { return lastModified; }
        public Map<String, String> getMetadata() { return metadata; }
    }

    /**
     * Custom exception for file operations.
     */
    public static class FileOperationException extends Exception {
        public FileOperationException(String message) {
            super(message);
        }

        public FileOperationException(String message, Throwable cause) {
            super(message, cause);
        }
    }

    /**
     * Example usage demonstration.
     */
    public static void main(String[] args) {
        FileMetadataManager manager = new FileMetadataManager();

        try {
            // Create a text file with metadata
            Map<String, String> metadata = new HashMap<>();
            metadata.put("author", "Development Team");
            metadata.put("version", "1.0");
            metadata.put("project", "File Management System");

            manager.createTextFile("documents/example.txt",
                    "This is professional file content.", metadata);

            // Retrieve the file
            FileContent retrieved = manager.retrieveTextFile("documents/example.txt");
            System.out.println("Content: " + retrieved.getContent());
            System.out.println("Metadata: " + retrieved.getMetadata());

        } catch (FileOperationException e) {
            LOGGER.log(Level.SEVERE, "Operation failed", e);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}
