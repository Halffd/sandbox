using System;
using System.IO;
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Threading;
using HtmlAgilityPack;

public class Scraper 
{
    public static void Run(string baseUrl)
    {
        try
        {
            Directory.CreateDirectory("thread");
            
            var web = new HtmlWeb();
            var doc = web.Load(baseUrl);
            
            var imageNodes = doc.DocumentNode.SelectNodes("//div[@class='fileText']/a");
            
            if (imageNodes != null)
            {
                using var client = new HttpClient();
                client.DefaultRequestHeaders.Add("User-Agent", "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36");
                
                foreach (var node in imageNodes)
                {
                    int retryCount = 0;
                    int maxRetries = 3;
                    int delay = 5000;
                    
                    while (retryCount < maxRetries)
                    {
                        try
                        {
                            string imageUrl = "https:" + node.Attributes["href"].Value;
                            string fileName = imageUrl.Split("/").Last();
                            
                            var response = client.GetAsync(imageUrl).Result;
                            
                            if (response.IsSuccessStatusCode)
                            {
                                var content = response.Content.ReadAsByteArrayAsync().Result;
                                File.WriteAllBytes("thread/" + fileName, content);
                                Console.WriteLine($"Downloaded: {fileName}");
                                break;
                            }
                            else if (response.StatusCode == HttpStatusCode.TooManyRequests)
                            {
                                throw new HttpRequestException("429 Too Many Requests");
                            }
                        }
                        catch (HttpRequestException ex) when (ex.Message.Contains("429"))
                        {
                            retryCount++;
                            Console.WriteLine($"Rate limited. Waiting {delay}ms before retry {retryCount}/{maxRetries}");
                            Thread.Sleep(delay);
                            delay *= 2;
                        }
                        catch (Exception ex)
                        {
                            Console.WriteLine($"Failed to download: {ex.Message}");
                            break;
                        }
                    }
                    
                    Thread.Sleep(2000); // Be nice between files
                }
            }
        }
        catch (Exception e)
        {
            Console.WriteLine($"Error: {e.Message}");
        }
        
        Console.WriteLine("Press any key to exit...");
        Console.ReadKey();
    }
}