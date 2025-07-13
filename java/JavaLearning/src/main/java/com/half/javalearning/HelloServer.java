/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package com.half.javalearning;
import java.io.*;
import java.net.*;

public class HelloServer {
    private static final int PORT = 1357;
    
    public static void start(String[] args) {
        try (ServerSocket serverSocket = new ServerSocket(PORT)) {
            System.out.println("Server started on port " + PORT);
            System.out.println("Listening on IP: " + InetAddress.getLocalHost().getHostAddress());
            
            while (true) {
                // Accept new connections
                Socket clientSocket = serverSocket.accept();
                System.out.println("New connection from: " + clientSocket.getInetAddress());
                
                // Handle each connection in a separate thread
                new Thread(() -> handleClient(clientSocket)).start();
            }
        } catch (IOException e) {
            System.err.println("Server error: " + e.getMessage());
        }
    }
    
    private static void handleClient(Socket clientSocket) {
        try (BufferedReader in = new BufferedReader(new InputStreamReader(clientSocket.getInputStream()));
             PrintWriter out = new PrintWriter(clientSocket.getOutputStream(), true)) {
            
            String inputLine;
            while ((inputLine = in.readLine()) != null) {
                if ("HELLO".equals(inputLine)) {
                    out.println("HELLO BACK");
                    System.out.println("Responded to HELLO from " + clientSocket.getInetAddress());
                } else {
                    out.println("UNKNOWN COMMAND");
                }
            }
        } catch (IOException e) {
            System.err.println("Client handling error: " + e.getMessage());
        } finally {
            try {
                clientSocket.close();
            } catch (IOException e) {
                System.err.println("Error closing socket: " + e.getMessage());
            }
        }
    }
}