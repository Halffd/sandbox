module com.half.javafxapp {
    requires javafx.controls;
    requires javafx.fxml;


    opens com.half.javafxapp to javafx.fxml;
    exports com.half.javafxapp;
}