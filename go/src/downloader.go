
package main

import (
    "fmt"
    "io"
    "net/http"
    "os"
)

func downloadFile(url string, filepath string) error {
    // Create the file
    out, err := os.Create(filepath)
    if err != nil {
        return err
    }
    defer out.Close()

    // Get the data
    resp, err := http.Get(url)
    if err != nil {
        return err
    }
    defer resp.Body.Close()

    // Check for a successful response
    if resp.StatusCode != http.StatusOK {
        return fmt.Errorf("failed to download file: %s", resp.Status)
    }

    // Write the body to file
    _, err = io.Copy(out, resp.Body)
    return err
}

func main() {
    // Replace this URL with the actual video URL you want to download
    videoURL := "https://www.example.com"
    filepath := "downloaded_video.mp4" // Change the filename as needed

    err := downloadFile(videoURL, filepath)
    if err != nil {
        fmt.Printf("Error downloading file: %v\n", err)
        return
    }

    fmt.Println("Download completed!")
}
