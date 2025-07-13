package com.half.javalearning.db;

import java.math.BigDecimal;
import java.sql.*;
import org.h2.tools.Server;

public class TestCity {
    private static final String DB_URL = "jdbc:h2:tcp://localhost:9092/~/citydb_new";
    private static final String USER = "sa";
    private static final String PASSWORD = "";

    public static void main(String[] args) {
        System.out.println("=== CONNECTING TO citydb@localhost ===");
        System.out.println("Database: citydb_new");
        System.out.println("URL: " + DB_URL);

        Server tcpServer = null;

        try {
            // Kill any existing H2 processes first
            System.out.println("\n🧹 Cleaning up any existing H2 processes...");
            cleanupExistingConnections();

            // Start H2 TCP server
            System.out.println("\n🚀 Starting H2 TCP server...");
            tcpServer = Server.createTcpServer(
                    "-tcpPort", "9092",
                    "-tcpAllowOthers",
                    "-ifNotExists",
                    "-baseDir", System.getProperty("user.home")
            ).start();

            System.out.println("✅ H2 TCP server started on port 9092");
            System.out.println("📁 Base directory: " + System.getProperty("user.home"));

            // Give server a moment to fully start
            Thread.sleep(1000);

            // Now connect
            try (Connection conn = DriverManager.getConnection(DB_URL, USER, PASSWORD)) {
                System.out.println("✅ Connected to database successfully");

                // Verify connection
                verifyConnection(conn);

                // Create tables if they don't exist
                createTablesIfNeeded(conn);

                // Show database info
                showDatabaseInfo(conn);

                // Your housing market fixes
                fixHousingMarketWithDataSource(conn);

            } catch (SQLException e) {
                System.err.println("💥 Database operations failed: " + e.getMessage());

                // Try to recover
                if (e.getMessage().contains("already in use") || e.getMessage().contains("locked")) {
                    System.out.println("\n🔄 Database locked, trying embedded mode...");
                    tryEmbeddedMode();
                } else {
                    e.printStackTrace();
                }
            }

        } catch (SQLException e) {
            System.err.println("💥 Failed to start H2 TCP server: " + e.getMessage());

            // Try embedded mode as fallback
            System.out.println("\n🔄 Trying embedded mode instead...");
            tryEmbeddedMode();

        } catch (InterruptedException e) {
            System.err.println("Thread interrupted: " + e.getMessage());
        } finally {
            // Clean shutdown
            if (tcpServer != null) {
                System.out.println("\n🛑 Stopping H2 TCP server...");
                tcpServer.stop();
                System.out.println("✅ Server stopped");
            }
        }
    }

    private static void cleanupExistingConnections() {
        try {
            // Try to shutdown any existing H2 servers
            Server.shutdownTcpServer("tcp://localhost:9092", "", true, true);
            Thread.sleep(500);
        } catch (Exception e) {
            // Ignore - probably no server running
        }
    }

    private static void tryEmbeddedMode() {
        System.out.println("Attempting embedded connection with unique database...");

        // Use timestamp to make it unique
        long timestamp = System.currentTimeMillis();
        String embeddedUrl = "jdbc:h2:~/citydb_embedded_" + timestamp + ";AUTO_SERVER=TRUE";

        try (Connection conn = DriverManager.getConnection(embeddedUrl, USER, PASSWORD)) {
            System.out.println("✅ Connected in embedded mode");
            System.out.println("📁 Database: citydb_embedded_" + timestamp);

            verifyConnection(conn);
            createTablesIfNeeded(conn);
            fixHousingMarketWithDataSource(conn);

        } catch (SQLException e) {
            System.err.println("💥 Embedded mode also failed: " + e.getMessage());

            // Last resort - in-memory database
            System.out.println("\n🔄 Trying in-memory database (data will be lost on exit)...");
            tryInMemoryMode();
        }
    }

    private static void tryInMemoryMode() {
        String memoryUrl = "jdbc:h2:mem:citydb_temp;DB_CLOSE_DELAY=-1";

        try (Connection conn = DriverManager.getConnection(memoryUrl, USER, PASSWORD)) {
            System.out.println("✅ Connected to in-memory database");
            System.out.println("⚠️  Data will be lost when program exits");

            verifyConnection(conn);
            createTablesIfNeeded(conn);
            fixHousingMarketWithDataSource(conn);

        } catch (SQLException e) {
            System.err.println("💥 Even in-memory mode failed: " + e.getMessage());
            System.err.println("🤷 Your H2 installation might be fucked");
        }
    }

    private static void createTablesIfNeeded(Connection conn) throws SQLException {
        System.out.println("\n=== CREATING TABLES IF NEEDED ===");

        // Check if tables exist
        DatabaseMetaData meta = conn.getMetaData();
        try (ResultSet tables = meta.getTables(null, "PUBLIC", "HOUSE", new String[]{"TABLE"})) {

            if (tables.next()) {
                System.out.println("✅ Tables already exist");
                return;
            }
        }

        // Create tables
        try (Statement stmt = conn.createStatement()) {

            // Prefecture table
            stmt.execute("""
                CREATE TABLE IF NOT EXISTS prefecture (
                    id INT PRIMARY KEY AUTO_INCREMENT,
                    name VARCHAR(100) NOT NULL,
                    population INT,
                    area_km2 DOUBLE,
                    established_year INT
                )
            """);

            // House table
            stmt.execute("""
                CREATE TABLE IF NOT EXISTS house (
                    id INT PRIMARY KEY AUTO_INCREMENT,
                    address VARCHAR(200) NOT NULL,
                    type VARCHAR(50) NOT NULL,
                    bedrooms INT,
                    bathrooms INT,
                    price DECIMAL(12,2),
                    residents INT,
                    prefecture_id INT,
                    FOREIGN KEY (prefecture_id) REFERENCES prefecture(id)
                )
            """);

            System.out.println("✅ Tables created");

            // Insert sample data
            insertSampleData(conn);
        }
    }

    private static void insertSampleData(Connection conn) throws SQLException {
        System.out.println("🏗️ Inserting sample data...");

        // Insert prefecture
        try (PreparedStatement pstmt = conn.prepareStatement(
                "INSERT INTO prefecture (name, population, area_km2, established_year) VALUES (?, ?, ?, ?)")) {

            pstmt.setString(1, "Tokyo");
            pstmt.setInt(2, 14000000);
            pstmt.setDouble(3, 2194.07);
            pstmt.setInt(4, 1943);
            pstmt.executeUpdate();
        }

        // Insert houses with our classic overpriced properties
        try (PreparedStatement pstmt = conn.prepareStatement(
                "INSERT INTO house (address, type, bedrooms, bathrooms, price, residents, prefecture_id) VALUES (?, ?, ?, ?, ?, ?, ?)")) {

            // Harajuku weeb tax apartment
            pstmt.setString(1, "123 Harajuku Street");
            pstmt.setString(2, "Apartment");
            pstmt.setInt(3, 2);
            pstmt.setInt(4, 1);
            pstmt.setBigDecimal(5, new BigDecimal("450000.00"));
            pstmt.setInt(6, 3);
            pstmt.setInt(7, 1);
            pstmt.executeUpdate();

            // Namba party pad
            pstmt.setString(1, "456 Namba Avenue");
            pstmt.setString(2, "Condo");
            pstmt.setInt(3, 3);
            pstmt.setInt(4, 2);
            pstmt.setBigDecimal(5, new BigDecimal("320000.00"));
            pstmt.setInt(6, 4);
            pstmt.setInt(7, 1);
            pstmt.executeUpdate();

            // Definitely not haunted traditional house
            pstmt.setString(1, "789 Bamboo Grove Road");
            pstmt.setString(2, "Traditional House");
            pstmt.setInt(3, 4);
            pstmt.setInt(4, 2);
            pstmt.setBigDecimal(5, new BigDecimal("280000.00"));
            pstmt.setInt(6, 5);
            pstmt.setInt(7, 1);
            pstmt.executeUpdate();
        }

        System.out.println("✅ Sample weeb housing data inserted");
    }

    private static void verifyConnection(Connection conn) throws SQLException {
        System.out.println("\n=== CONNECTION VERIFICATION ===");

        DatabaseMetaData meta = conn.getMetaData();
        System.out.println("✅ Connected to: " + meta.getDatabaseProductName());
        System.out.println("📊 Version: " + meta.getDatabaseProductVersion());
        System.out.println("🔗 Driver: " + meta.getDriverName() + " v" + meta.getDriverVersion());
        System.out.println("🏠 URL: " + meta.getURL());

        // Test query
        try (Statement stmt = conn.createStatement();
             ResultSet rs = stmt.executeQuery("SELECT 1 as test")) {

            if (rs.next()) {
                System.out.println("🎯 Test query: SUCCESS");
            }
        }
    }

    private static void showDatabaseInfo(Connection conn) throws SQLException {
        System.out.println("\n=== DATABASE SCHEMA INFO ===");

        DatabaseMetaData meta = conn.getMetaData();
        try (ResultSet tables = meta.getTables(null, "PUBLIC", null, new String[]{"TABLE"})) {

            System.out.println("Tables:");
            while (tables.next()) {
                String tableName = tables.getString("TABLE_NAME");
                System.out.println("  📋 " + tableName);

                // Count rows
                try (Statement stmt = conn.createStatement();
                     ResultSet rs = stmt.executeQuery("SELECT COUNT(*) FROM " + tableName)) {

                    if (rs.next()) {
                        System.out.println("     └── " + rs.getInt(1) + " rows");
                    }
                }
            }
        }
    }

    private static void fixHousingMarketWithDataSource(Connection conn) throws SQLException {
        System.out.println("\n=== FIXING OVERPRICED WEEB HOUSING ===");

        // Show original prices
        System.out.println("\nBEFORE (overpriced bullshit):");
        showHousingPrices(conn);

        // Fix the Harajuku ripoff
        String updateHarajuku = """
            UPDATE house 
            SET price = ?, 
                residents = ?,
                address = ?
            WHERE address LIKE '%Harajuku%'
        """;

        try (PreparedStatement pstmt = conn.prepareStatement(updateHarajuku)) {
            pstmt.setBigDecimal(1, new BigDecimal("275000.00"));
            pstmt.setInt(2, 2);
            pstmt.setString(3, "123 Harajuku Street (Weeb Tax Reduced)");

            int updated = pstmt.executeUpdate();
            System.out.println("🏠 Updated " + updated + " overpriced Harajuku apartment");
        }

        // Show fixed prices
        System.out.println("\nAFTER (slightly less ridiculous):");
        showHousingPrices(conn);
    }

    private static void showHousingPrices(Connection conn) throws SQLException {
        String query = """
            SELECT address, type, bedrooms, bathrooms, price, residents,
                   ROUND(price / residents, 0) as price_per_person,
                   ROUND(residents * 1.0 / bathrooms, 1) as people_per_bathroom
            FROM house 
            ORDER BY price DESC
        """;

        try (Statement stmt = conn.createStatement();
             ResultSet rs = stmt.executeQuery(query)) {

            System.out.printf("%-40s | %-15s | %s | %8s | %s | %s | %s%n",
                    "Address", "Type", "BR/BA", "Price", "Residents", "$/Person", "PPB");
            System.out.println("-".repeat(110));

            while (rs.next()) {
                System.out.printf("%-40s | %-15s | %d/%d   | $%,6.0f | %9d | $%,6.0f | %4.1f%n",
                        rs.getString("address"),
                        rs.getString("type"),
                        rs.getInt("bedrooms"),
                        rs.getInt("bathrooms"),
                        rs.getBigDecimal("price").doubleValue(),
                        rs.getInt("residents"),
                        rs.getBigDecimal("price_per_person").doubleValue(),
                        rs.getDouble("people_per_bathroom"));
            }
        }
    }
}