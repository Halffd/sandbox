package com.half.javalearning.db;

import javax.sql.DataSource;
import java.sql.*;
import java.util.Properties;

public class CityDBDataSource {
    private static final String DB_URL = "jdbc:h2:tcp://localhost:9092/citydb";
    private static final String USER = "sa";
    private static final String PASSWORD = "123";

    // Simple connection getter
    public static Connection getConnection() throws SQLException {
        return DriverManager.getConnection(DB_URL, USER, PASSWORD);
    }

    // Properties-based connection
    public static Connection getConnectionWithProperties() throws SQLException {
        Properties props = new Properties();
        props.setProperty("user", USER);
        props.setProperty("password", PASSWORD);
        props.setProperty("autoReconnect", "true");
        props.setProperty("characterEncoding", "UTF-8");

        return DriverManager.getConnection(DB_URL, props);
    }

    // Connection with custom settings
    public static Connection getOptimizedConnection() throws SQLException {
        Properties props = new Properties();
        props.setProperty("user", USER);
        props.setProperty("password", PASSWORD);
        props.setProperty("DB_CLOSE_DELAY", "-1"); // Keep DB open
        props.setProperty("CACHE_SIZE", "65536"); // 64MB cache
        props.setProperty("LOCK_TIMEOUT", "10000"); // 10 second timeout

        return DriverManager.getConnection(DB_URL, props);
    }
}
