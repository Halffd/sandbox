package com.half.javalearning.video;

// Custom Exception Classes
class VideoPlayerException extends Exception {
    public VideoPlayerException(String message) {
        super(message);
    }

    public VideoPlayerException(String message, Throwable cause) {
        super(message, cause);
    }
}

class FileNotFoundException extends VideoPlayerException {
    public FileNotFoundException(String message) {
        super(message);
    }

    public FileNotFoundException(String message, Throwable cause) {
        super(message, cause);
    }
}

class UnsupportedFormatException extends VideoPlayerException {
    public UnsupportedFormatException(String message) {
        super(message);
    }

    public UnsupportedFormatException(String message, Throwable cause) {
        super(message, cause);
    }
}

class PlaybackException extends VideoPlayerException {
    public PlaybackException(String message) {
        super(message);
    }

    public PlaybackException(String message, Throwable cause) {
        super(message, cause);
    }
}