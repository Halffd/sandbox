package com.half.javalearning.ui.swing;

import javax.swing.JFrame;

public class Swing extends javax.swing.JPanel {

    public Swing() {
        initComponents();
    }

    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">
    private void initComponents() {
        jButton1 = new javax.swing.JButton();
        jLabel1 = new javax.swing.JLabel();
        jButton2 = new javax.swing.JButton();

        jButton1.setText("jButton1");

        setForeground(new java.awt.Color(51, 51, 51));

        jLabel1.setFont(new java.awt.Font("VL Gothic", 1, 48));
        jLabel1.setForeground(new java.awt.Color(0, 153, 255));
        jLabel1.setText("Swing");

        jButton2.setBackground(new java.awt.Color(51, 51, 0));
        jButton2.setFont(new java.awt.Font("Fira Sans", 2, 24));
        jButton2.setForeground(new java.awt.Color(102, 255, 102));
        jButton2.setText("Click");
        jButton2.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton2ActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
                layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(layout.createSequentialGroup()
                                .addGap(115, 115, 115)
                                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                        .addComponent(jLabel1)
                                        .addGroup(layout.createSequentialGroup()
                                                .addGap(24, 24, 24)
                                                .addComponent(jButton2)))
                                .addContainerGap(157, Short.MAX_VALUE))
        );
        layout.setVerticalGroup(
                layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(layout.createSequentialGroup()
                                .addGap(27, 27, 27)
                                .addComponent(jLabel1)
                                .addGap(58, 58, 58)
                                .addComponent(jButton2)
                                .addContainerGap(130, Short.MAX_VALUE))
        );
    }// </editor-fold>

    private void jButton2ActionPerformed(java.awt.event.ActionEvent evt) {
        // If label was clicked, change its text, else change the button's text
        if (jLabel1.getText().equals("Swing")) {
            jLabel1.setText("Hello World!");
            jLabel1.setForeground(new java.awt.Color(255, 0, 0));
            jLabel1.setBackground(new java.awt.Color(0, 0, 255));
            jLabel1.setFont(new java.awt.Font("Arial", 1, 48));
            jLabel1.setOpaque(true);
            jLabel1.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(0, 0, 0), 1));
            jLabel1.setCursor(new java.awt.Cursor(java.awt.Cursor.HAND_CURSOR));
        } else {
            jLabel1.setText("Swing");
            jLabel1.setForeground(new java.awt.Color(0, 153, 255));
            jLabel1.setBackground(new java.awt.Color(51, 51, 0));
            jLabel1.setFont(new java.awt.Font("Fira Sans", 2, 24));
            jLabel1.setOpaque(true);
            jLabel1.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(0, 0, 0), 1));
            jLabel1.setCursor(new java.awt.Cursor(java.awt.Cursor.HAND_CURSOR));
        }
    }

    // Variables declaration - do not modify
    private javax.swing.JButton jButton1;
    private javax.swing.JButton jButton2;
    private javax.swing.JLabel jLabel1;
    // End of variables declaration

    public static void main(String args[]) {
        try {
            for (javax.swing.UIManager.LookAndFeelInfo info : javax.swing.UIManager.getInstalledLookAndFeels()) {
                if ("Nimbus".equals(info.getName())) {
                    javax.swing.UIManager.setLookAndFeel(info.getClassName());
                    break;
                }
            }
        } catch (ClassNotFoundException | InstantiationException | IllegalAccessException | javax.swing.UnsupportedLookAndFeelException ex) {
            ex.printStackTrace();
        }

        java.awt.EventQueue.invokeLater(new Runnable() {
            public void run() {
                // Create a JFrame to hold our panel
                // Create a JFrame to hold our panel
                JFrame frame = new JFrame("Swing Demo");
                frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

// Create our panel and set its background
                Swing swingPanel = new Swing();
                swingPanel.setBackground(new java.awt.Color(0, 0, 0));
                frame.add(swingPanel);

// On some systems/look-and-feels, you might need this too
                frame.setBackground(new java.awt.Color(0, 0, 0));

// If you're using decorated frame (with title bar)
// The root pane also needs to be set
                frame.getRootPane().setBackground(new java.awt.Color(0, 0, 0));
                frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
                frame.add(new Swing());
                frame.pack();
                frame.setVisible(true);
            }
        });
    }
}