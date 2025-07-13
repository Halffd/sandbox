package scraper

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.{Duration, DurationInt}
import scala.util.{Failure, Success, Try}
import scala.jdk.CollectionConverters.*
import java.nio.file.{Files, Path, Paths}
import java.net.{URL, MalformedURLException}
import java.io.IOException
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import com.typesafe.scalalogging.LazyLogging

object SafeChanScraper extends LazyLogging {
  
  // Use a proper thread pool instead of global context
  given ExecutionContext = ExecutionContext.fromExecutor(
    java.util.concurrent.Executors.newFixedThreadPool(8)
  )
  
  case class ScrapingConfig(
    downloadDirectory: Path = Paths.get("downloads"),
    maxConcurrentDownloads: Int = 10,
    requestTimeout: Duration = 30.seconds,
    userAgent: String = "Mozilla/5.0 (compatible; SafeChanScraper/1.0)"
  )
  
  case class DownloadResult(
    url: String,
    fileName: String,
    success: Boolean,
    errorMessage: Option[String] = None
  )
  
  case class PostData(
    number: String,
    name: String,
    timestamp: String,
    postId: String,
    content: String,
    fileInfo: Option[String]
  )
  
  def validateUrl(urlString: String): Either[String, URL] = {
    Try(new URL(urlString)) match {
      case Success(url) if url.getProtocol == "https" => Right(url)
      case Success(_) => Left("Only HTTPS URLs are allowed")
      case Failure(_) => Left(s"Invalid URL format: $urlString")
    }
  }
  
  def sanitizeFileName(fileName: String): String = {
    val sanitized = fileName.replaceAll("[^a-zA-Z0-9._-]", "_")
    if (sanitized.length > 255) sanitized.take(255) else sanitized
  }
  
  def ensureDirectoryExists(path: Path): Either[String, Path] = {
    Try {
      if (!Files.exists(path)) {
        Files.createDirectories(path)
      }
      path
    }.toEither.left.map(_.getMessage)
  }
  
  def downloadFile(
    url: String, 
    targetDirectory: Path,
    config: ScrapingConfig
  ): Future[DownloadResult] = {
    
    Future {
      logger.info(s"Starting download: $url")
      
      val result = for {
        validUrl <- validateUrl(url)
        _ <- ensureDirectoryExists(targetDirectory)
        fileName = sanitizeFileName(validUrl.getPath.split("/").lastOption.getOrElse("unknown"))
        fullPath = targetDirectory.resolve(fileName)
        
        downloadResult <- Try {
          if (Files.exists(fullPath)) {
            logger.warn(s"File already exists, skipping: $fileName")
            DownloadResult(url, fileName, success = true)
          } else {
            val response = requests.get(
              url,
              headers = Map("User-Agent" -> config.userAgent),
              readTimeout = config.requestTimeout.toMillis.toInt,
              connectTimeout = 10000
            )
            
            if (response.statusCode == 200) {
              Files.write(fullPath, response.bytes)
              logger.info(s"Successfully downloaded: $fileName")
              DownloadResult(url, fileName, success = true)
            } else {
              val error = s"HTTP ${response.statusCode}: ${response.statusMessage}"
              logger.error(s"Download failed for $url: $error")
              DownloadResult(url, fileName, success = false, Some(error))
            }
          }
        }.toEither.left.map(_.getMessage)
        
      } yield downloadResult
      
      result match {
        case Right(downloadResult) => downloadResult
        case Left(error) => 
          logger.error(s"Download failed for $url: $error")
          DownloadResult(url, "unknown", success = false, Some(error))
      }
    }
  }
  
  def extractMediaUrls(threadUrl: String): Either[String, List[String]] = {
    Try {
      logger.info(s"Fetching thread: $threadUrl")
      
      val doc: Document = Jsoup.connect(threadUrl)
        .userAgent("Mozilla/5.0 (compatible; SafeChanScraper/1.0)")
        .timeout(30000)
        .get()
      
      val mediaLinks = doc.select("img")
        .asScala
        .map(_.attr("src"))
        .filter(_.nonEmpty)
        .map(href => if (href.startsWith("//")) s"https:$href" else href)
        .filter(url => validateUrl(url).isRight)
        .toList
      
      logger.info(s"Found ${mediaLinks.size} media files")
      mediaLinks
      
    }.toEither.left.map(ex => s"Failed to extract URLs: ${ex.getMessage}")
  }
  
  def scrapeThread(
    threadUrl: String, 
    config: ScrapingConfig = ScrapingConfig()
  ): Future[List[DownloadResult]] = {
    
    extractMediaUrls(threadUrl) match {
      case Left(error) => 
        logger.error(error)
        Future.successful(List.empty)
        
      case Right(mediaUrls) =>
        logger.info(s"Starting download of ${mediaUrls.size} files")
        
        // Batch downloads to prevent overwhelming the server
        val batches = mediaUrls.grouped(config.maxConcurrentDownloads).toList
        
        def processBatch(batch: List[String]): Future[List[DownloadResult]] = {
          val downloads = batch.map(url => downloadFile(url, config.downloadDirectory, config))
          Future.sequence(downloads)
        }
        
        // Process batches sequentially to be respectful
        batches.foldLeft(Future.successful(List.empty[DownloadResult])) { (acc, batch) =>
          for {
            previous <- acc
            current <- processBatch(batch)
            _ <- Future { Thread.sleep(1000) } // Be nice to the server
          } yield previous ++ current
        }
    }
  }
  
  def saveTexts(threadUrl: String, config: ScrapingConfig): Either[String, Path] = {
    Try {
      logger.info(s"Scraping text content from: $threadUrl")
      
      val doc: Document = Jsoup.connect(threadUrl)
        .userAgent(config.userAgent)
        .timeout(config.requestTimeout.toMillis.toInt)
        .get()
      
      // Extract thread info
      val threadTitle = doc.select(".subject").asScala.headOption
        .map(_.text().trim)
        .filter(_.nonEmpty)
        .getOrElse("No Subject")
      
      val threadId = threadUrl.split("/").lastOption.getOrElse("unknown")
      
      // Extract all posts
      val posts = doc.select(".post").asScala.toList.zipWithIndex.map { case (post, index) =>
        val postNumber = post.attr("id").replace("p", "")
        val name = post.select(".name").text().trim
        val timestamp = post.select(".dateTime").text().trim
        val postId = post.select(".posteruid .hand").text().trim
        
        // Extract post content, handling greentext and quotes
        val messageElement = post.select(".postMessage").first()
        val content = if (messageElement != null) {
          // Convert HTML to more readable text while preserving structure
          messageElement.select("br").before("\n")
          messageElement.select(".quote").before(">")
          messageElement.text()
            .split("\n")
            .map(line => if (line.trim.startsWith(">")) s">${line.trim.drop(1)}" else line)
            .mkString("\n")
        } else ""
        
        // Extract file info if present
        val fileInfo = post.select(".fileText").asScala.headOption.map { fileElement =>
          val fileName = fileElement.select("a").text().trim
          val fileSize = fileElement.text().replaceAll(fileName, "").trim
          s"File: $fileName $fileSize"
        }
        
        PostData(
          number = if (postNumber.nonEmpty) postNumber else s"post_$index",
          name = if (name.nonEmpty) name else "Anonymous",
          timestamp = timestamp,
          postId = postId,
          content = content.trim,
          fileInfo = fileInfo
        )
      }
      
      // Generate text file content
      val textContent = generateTextFile(threadTitle, threadId, threadUrl, posts)
      
      // Save to file
      val fileName = s"thread_${threadId}_${sanitizeFileName(threadTitle)}.txt"
      val filePath = config.downloadDirectory.resolve(fileName)
      
      Files.write(filePath, textContent.getBytes("UTF-8"))
      logger.info(s"Saved text content to: $fileName")
      
      filePath
      
    }.toEither.left.map(ex => s"Failed to save texts: ${ex.getMessage}")
  }
  
  def generateTextFile(
    threadTitle: String, 
    threadId: String, 
    threadUrl: String, 
    posts: List[PostData]
  ): String = {
    val header = s"""
|========================================
|Thread: $threadTitle
|ID: $threadId
|URL: $threadUrl
|Scraped: ${java.time.LocalDateTime.now()}
|Total Posts: ${posts.length}
|========================================
|
|""".stripMargin
    
    val postsText = posts.map { post =>
      val fileInfoText = post.fileInfo.map(info => s"\n$info\n").getOrElse("")
      
      s"""
|Post #${post.number} | ${post.name} | ${post.timestamp}${if (post.postId.nonEmpty) s" | ID: ${post.postId}" else ""}
|${"-" * 80}$fileInfoText
|${post.content}
|
|""".stripMargin
    }.mkString("\n")
    
    val footer = s"""
|========================================
|End of thread
|========================================
|""".stripMargin
    
    header + postsText + footer
  }
  
  def scrapeThreadWithTexts(
    threadUrl: String, 
    config: ScrapingConfig = ScrapingConfig(),
    saveTextContent: Boolean = true
  ): Future[(List[DownloadResult], Option[Path])] = {
    
    val mediaDownloadsFuture = scrapeThread(threadUrl, config)
    
    val textSaveFuture = if (saveTextContent) {
      Future {
        saveTexts(threadUrl, config) match {
          case Right(path) => Some(path)
          case Left(error) => 
            logger.error(s"Failed to save text content: $error")
            None
        }
      }
    } else {
      Future.successful(None)
    }
    
    for {
      downloads <- mediaDownloadsFuture
      textPath <- textSaveFuture
    } yield (downloads, textPath)
  }
  
  def printResults(results: List[DownloadResult]): Unit = {
    val successful = results.count(_.success)
    val failed = results.length - successful
    
    println(s"\n=== Download Summary ===")
    println(s"Total files: ${results.length}")
    println(s"Successful: $successful")
    println(s"Failed: $failed")
    
    if (failed > 0) {
      println(s"\nFailed downloads:")
      results.filter(!_.success).foreach { result =>
        println(s"  - ${result.url}: ${result.errorMessage.getOrElse("Unknown error")}")
      }
    }
    
    println(s"\nFiles saved to: ${results.headOption.map(_ => "img").getOrElse("No files downloaded")}")
  }
}
// Fixed main function with proper error handling
@main def runScraper(args: String*): Unit = {
  import SafeChanScraper.*
  import SafeChanScraper.given
  
  // Parse command line arguments
  val (threadUrl, config, saveTextContent) = args.toList match {
    case Nil => 
      println("Usage: runScraper <thread-url> [download-directory] [--text-only | --no-text]")
      System.exit(1)
      throw new RuntimeException("Unreachable")
      
    case url :: rest =>
      val dir = rest.find(!_.startsWith("--")).getOrElse("img")
      val textOnly = rest.contains("--text-only")
      val noText = rest.contains("--no-text")
      
      (url, ScrapingConfig(downloadDirectory = Paths.get(dir)), !noText)
  }
  
  // Validate URL format
  SafeChanScraper.validateUrl(threadUrl) match {
    case Left(error) =>
      println(s"Invalid URL: $error")
      System.exit(1)
    case Right(_) =>
      println(s"✓ Valid URL: $threadUrl")
  }
  
  println(s"📁 Download directory: ${config.downloadDirectory.toAbsolutePath}")
  println(s"📝 Save text content: $saveTextContent")
  println()
  
  // Create download directory if it doesn't exist
  SafeChanScraper.ensureDirectoryExists(config.downloadDirectory) match {
    case Left(error) =>
      println(s"❌ Failed to create download directory: $error")
      System.exit(1)
    case Right(_) =>
      println(s"✓ Download directory ready")
  }
  
  // Start scraping
  println(s"🚀 Starting scrape operation...")
  val startTime = System.currentTimeMillis()
  
  val scrapeResult = if (args.contains("--text-only")) {
    // Only save text content, no media downloads
    Future {
      SafeChanScraper.saveTexts(threadUrl, config) match {
        case Right(textPath) => 
          println(s"✅ Text content saved to: ${textPath.getFileName}")
          (List.empty[DownloadResult], Some(textPath))
        case Left(errorMsg) => 
          println(s"❌ Failed to save text: $errorMsg")
          (List.empty[DownloadResult], None)
      }
    }
  } else {
    SafeChanScraper.scrapeThreadWithTexts(threadUrl, config, saveTextContent)
  }
  
  scrapeResult.onComplete {
    case Success((results, textPath)) => 
      val endTime = System.currentTimeMillis()
      val duration = (endTime - startTime) / 1000.0
      
      println(s"⏱️  Operation completed in ${duration}s")
      
      if (results.nonEmpty) {
        SafeChanScraper.printResults(results)
      }
      
      textPath.foreach { path =>
        println(s"📝 Text content saved to: ${path.getFileName}")
      }
      
      println(s"🎉 Scraping completed successfully!")
      System.exit(0)
      
    case Failure(exception) => 
      println(s"❌ Scraping failed: ${exception.getMessage}")
      exception.printStackTrace()
      System.exit(1)
  }
  
  // Keep the main thread alive while downloads happen
  println("⏳ Waiting for downloads to complete...")
  Thread.sleep(300000) // 5 minutes max wait
  
  // If we reach here, something went wrong
  println("⚠️  Timeout reached, exiting...")
  System.exit(1)
}