package com.half.javafxapp;

import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.*;
import javafx.scene.paint.Color;

import java.util.Random;

public class HelloController {
    @FXML
    private Label welcomeText;
    @FXML
    private Button button;
    @FXML
    private Label lbl;
    private int i = 0;
    private final Random random = new Random();

    @FXML
    protected void onHelloButtonClick() {
        if (i >= 0) {
            button.setText("Click Again " + i + " times");
            i++;
            double c = Math.min(i * 25, 255) / 255.0;

            Border border = new Border(new BorderStroke(Color.color(c, 0, 0, 1.0),
                    BorderStrokeStyle.SOLID, CornerRadii.EMPTY, BorderWidths.DEFAULT));

            welcomeText.setBorder(border);
            lbl.setBorder(border);

            if (i == 3) {
                i = 0;
                welcomeText.setText("Welcome to JavaFX Application!");
                lbl.setText("Clicked 3 times!");
                welcomeText.setBorder(new Border(new BorderStroke(Color.RED,
                        BorderStrokeStyle.SOLID, CornerRadii.EMPTY, BorderWidths.DEFAULT)));
            } else {
                welcomeText.setText("Java");
                lbl.setText("Java");
                welcomeText.setBorder(new Border(new BorderStroke(Color.WHITE,
                        BorderStrokeStyle.SOLID, CornerRadii.EMPTY, BorderWidths.DEFAULT)));
            }
        }
    }

    @FXML
    protected void lblMouseMove(MouseEvent event) {
        lbl.setText("Mouse moved!");
        Color randomColor = Color.rgb(random.nextInt(256), random.nextInt(256), random.nextInt(256));
        lbl.setTextFill(randomColor);
    }
}
