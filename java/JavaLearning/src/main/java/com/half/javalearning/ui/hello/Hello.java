package com.half.javalearning.ui.hello;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.*;

public class Hello extends JFrame {
    public static void main(String[] args) {
        // Create and display the frame on the Event Dispatch Thread (important!)
        SwingUtilities.invokeLater(() -> {
            Hello app = new Hello();
            app.setVisible(true);
        });
    }

    // Constructor - this is where you set up your UI
    public Hello() {
        // Frame setup
        setTitle("Hello Swing");
        setSize(500, 400);
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setLocationRelativeTo(null);

        // Create a button for our examples
        JButton button = new JButton("Click Me");

        // Create a label to display results
        JLabel resultLabel = new JLabel("Results will appear here");

        // Set up the panel and add components
        JPanel panel = new JPanel();
        panel.add(button);
        panel.add(resultLabel);

        // Add the panel to *this* frame
        add(panel);  // Not "swing.add" - "this" is the frame!

        // The old painful way
        button.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                System.out.println("Why so verbose?");
                resultLabel.setText("Button clicked the verbose way");
            }
        });

        // Stream example with UI integration
        JButton streamButton = new JButton("Process Names");
        panel.add(streamButton);

        streamButton.addActionListener(e -> {
            List<String> names = Arrays.asList("Alice", "Bob", "Charlie");
            StringBuilder result = new StringBuilder("<html>");

            names.stream()
                    .filter(name -> name.length() > 3)
                    .map(String::toUpperCase)
                    .forEach(name -> result.append(name).append("<br>"));

            result.append("</html>");
            resultLabel.setText(result.toString());
        });
    }
}