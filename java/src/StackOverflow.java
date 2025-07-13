import java.io.*;
import java.net.*;
import java.util.*;

public class StackOverflow {
    public static void main(String[] args) throws Exception {
        // Actually read the file properly
        StringBuilder sourceCode = new StringBuilder();
        try (Scanner sc = new Scanner(new File("StackOverflow.java"))) {
            while (sc.hasNextLine()) {
                sourceCode.append(sc.nextLine()).append("\n");
            }
        }
        
        // Proper URL encoding
        String encodedCode = URLEncoder.encode("[code]\n" + sourceCode.toString() + "[/code]", "UTF-8");
        
        // Build proper POST data
        String postData = String.format(
            "bbs=prog&subj=%s&com=%s", 
            URLEncoder.encode("ITT: Fixed self-posting program", "UTF-8"),
            encodedCode
        );
        
        // Use proper HTTP connection
        URL url = new URL("http://dis.4chan.org/post");
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        conn.setRequestMethod("POST");
        conn.setRequestProperty("Content-Type", "application/x-www-form-urlencoded");
        conn.setRequestProperty("User-Agent", "Gibson/9000");
        conn.setDoOutput(true);
        
        // Send the data
        try (OutputStreamWriter out = new OutputStreamWriter(conn.getOutputStream())) {
            out.write(postData);
        }
        
        // Read response
        try (BufferedReader in = new BufferedReader(new InputStreamReader(conn.getInputStream()))) {
            String line;
            while ((line = in.readLine()) != null) {
                System.out.println(line);
            }
        }
    }
}