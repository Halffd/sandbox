package com.half.javalearning.ui;

import java.awt.*;
import java.awt.event.*;

public class AWT {
    public static void main(String[] args) {
        Frame f = new Frame("AWT");
        f.setSize(400, 400);
        f.setLocation(100, 100);
        f.setLayout(new FlowLayout()); // Set layout manager

        Label l = new Label("Hello, AWT!");
        f.add(l);

        Button b = new Button("Click me");
        b.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                System.out.println("Button clicked!");
            }
        });
        f.add(b);

        Checkbox c = new Checkbox("Checkbox");
        f.add(c);

        // Example of using a dropdown (ComboBox)
        Choice d = new Choice();
        d.add("Option 1");
        d.add("Option 2");
        d.add("Option 3");
        f.add(d);

        f.addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent we) {
                System.exit(0);
            }
        });

        f.setVisible(true);
    }
}