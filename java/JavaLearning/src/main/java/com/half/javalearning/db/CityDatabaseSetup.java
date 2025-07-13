package com.half.javalearning.db;

import java.sql.*;
import java.util.ArrayList;
import java.util.List;

abstract class GenericDAO<E,K> {
    protected Connection connection;

    public GenericDAO(Connection connection) {
        this.connection = connection;
    }

    public abstract void include(E entity);
    public abstract void exclude(K key);
    public abstract void change(E entity);
    public abstract E get(K key);
    public abstract List<E> getAll();
}

class Prefecture {
    int id;
    String name;
    int population;
    double areaKm2;
    int establishedYear;

    public Prefecture(int id, String name, int population, double areaKm2, int establishedYear) {
        this.id = id;
        this.name = name;
        this.population = population;
        this.areaKm2 = areaKm2;
        this.establishedYear = establishedYear;
    }

    @Override
    public String toString() {
        return String.format("Prefecture{id=%d, name='%s', population=%,d, area=%.2f km², established=%d}",
                id, name, population, areaKm2, establishedYear);
    }
}

class PrefectureDAO extends GenericDAO<Prefecture, Integer> {

    public PrefectureDAO(Connection connection) {
        super(connection);
    }

    @Override
    public void include(Prefecture entity) {
        String sql = "INSERT INTO prefecture (name, population, area_km2, established_year) VALUES (?, ?, ?, ?)";
        try (PreparedStatement ps = connection.prepareStatement(sql)) {
            ps.setString(1, entity.name);
            ps.setInt(2, entity.population);
            ps.setDouble(3, entity.areaKm2);
            ps.setInt(4, entity.establishedYear);
            ps.executeUpdate();
        } catch (SQLException e) {
            throw new RuntimeException("Failed to insert prefecture", e);
        }
    }

    @Override
    public void exclude(Integer key) {
        String sql = "DELETE FROM prefecture WHERE id = ?";
        try (PreparedStatement ps = connection.prepareStatement(sql)) {
            ps.setInt(1, key);
            ps.executeUpdate();
        } catch (SQLException e) {
            throw new RuntimeException("Failed to delete prefecture", e);
        }
    }

    @Override
    public void change(Prefecture entity) {
        String sql = "UPDATE prefecture SET name = ?, population = ?, area_km2 = ?, established_year = ? WHERE id = ?";
        try (PreparedStatement ps = connection.prepareStatement(sql)) {
            ps.setString(1, entity.name);
            ps.setInt(2, entity.population);
            ps.setDouble(3, entity.areaKm2);
            ps.setInt(4, entity.establishedYear);
            ps.setInt(5, entity.id);
            ps.executeUpdate();
        } catch (SQLException e) {
            throw new RuntimeException("Failed to update prefecture", e);
        }
    }

    @Override
    public Prefecture get(Integer key) {
        String sql = "SELECT * FROM prefecture WHERE id = ?";
        try (PreparedStatement ps = connection.prepareStatement(sql)) {
            ps.setInt(1, key);
            ResultSet rs = ps.executeQuery();
            if (rs.next()) {
                return new Prefecture(
                        rs.getInt("id"),
                        rs.getString("name"),
                        rs.getInt("population"),
                        rs.getDouble("area_km2"),
                        rs.getInt("established_year")
                );
            }
        } catch (SQLException e) {
            throw new RuntimeException("Failed to get prefecture", e);
        }
        return null;
    }

    @Override
    public List<Prefecture> getAll() {
        List<Prefecture> prefectures = new ArrayList<>();
        String sql = "SELECT * FROM prefecture ORDER BY name";
        try (Statement stmt = connection.createStatement();
             ResultSet rs = stmt.executeQuery(sql)) {

            while (rs.next()) {
                prefectures.add(new Prefecture(
                        rs.getInt("id"),
                        rs.getString("name"),
                        rs.getInt("population"),
                        rs.getDouble("area_km2"),
                        rs.getInt("established_year")
                ));
            }
        } catch (SQLException e) {
            throw new RuntimeException("Failed to get all prefectures", e);
        }
        return prefectures;
    }
}

class School {
    int id;
    String name;
    String type;
    int students;
    String address;
    int prefectureId;

    public School(int id, String name, String type, int students, String address, int prefectureId) {
        this.id = id;
        this.name = name;
        this.type = type;
        this.students = students;
        this.address = address;
        this.prefectureId = prefectureId;
    }

    @Override
    public String toString() {
        return String.format("School{id=%d, name='%s', type='%s', students=%d, address='%s', prefectureId=%d}",
                id, name, type, students, address, prefectureId);
    }
}

class SchoolDAO extends GenericDAO<School, Integer> {

    public SchoolDAO(Connection connection) {
        super(connection);
    }

    @Override
    public void include(School entity) {
        String sql = "INSERT INTO school (name, type, students, address, prefecture_id) VALUES (?, ?, ?, ?, ?)";
        try (PreparedStatement ps = connection.prepareStatement(sql)) {
            ps.setString(1, entity.name);
            ps.setString(2, entity.type);
            ps.setInt(3, entity.students);
            ps.setString(4, entity.address);
            ps.setInt(5, entity.prefectureId);
            ps.executeUpdate();
        } catch (SQLException e) {
            throw new RuntimeException("Failed to insert school", e);
        }
    }

    @Override
    public void exclude(Integer key) {
        String sql = "DELETE FROM school WHERE id = ?";
        try (PreparedStatement ps = connection.prepareStatement(sql)) {
            ps.setInt(1, key);
            ps.executeUpdate();
        } catch (SQLException e) {
            throw new RuntimeException("Failed to delete school", e);
        }
    }

    @Override
    public void change(School entity) {
        String sql = "UPDATE school SET name = ?, type = ?, students = ?, address = ?, prefecture_id = ? WHERE id = ?";
        try (PreparedStatement ps = connection.prepareStatement(sql)) {
            ps.setString(1, entity.name);
            ps.setString(2, entity.type);
            ps.setInt(3, entity.students);
            ps.setString(4, entity.address);
            ps.setInt(5, entity.prefectureId);
            ps.setInt(6, entity.id);
            ps.executeUpdate();
        } catch (SQLException e) {
            throw new RuntimeException("Failed to update school", e);
        }
    }

    @Override
    public School get(Integer key) {
        String sql = "SELECT * FROM school WHERE id = ?";
        try (PreparedStatement ps = connection.prepareStatement(sql)) {
            ps.setInt(1, key);
            ResultSet rs = ps.executeQuery();
            if (rs.next()) {
                return new School(
                        rs.getInt("id"),
                        rs.getString("name"),
                        rs.getString("type"),
                        rs.getInt("students"),
                        rs.getString("address"),
                        rs.getInt("prefecture_id")
                );
            }
        } catch (SQLException e) {
            throw new RuntimeException("Failed to get school", e);
        }
        return null;
    }

    @Override
    public List<School> getAll() {
        List<School> schools = new ArrayList<>();
        String sql = "SELECT * FROM school ORDER BY name";
        try (Statement stmt = connection.createStatement();
             ResultSet rs = stmt.executeQuery(sql)) {

            while (rs.next()) {
                schools.add(new School(
                        rs.getInt("id"),
                        rs.getString("name"),
                        rs.getString("type"),
                        rs.getInt("students"),
                        rs.getString("address"),
                        rs.getInt("prefecture_id")
                ));
            }
        } catch (SQLException e) {
            throw new RuntimeException("Failed to get all schools", e);
        }
        return schools;
    }
}

public class CityDatabaseSetup {
    private static final String DB_URL = "jdbc:h2:~/citydb;AUTO_SERVER=TRUE";
    private static final String USER = "sa";
    private static final String PASSWORD = "";

    public static void main(String[] args) {
        try {
            org.h2.tools.Server webServer = org.h2.tools.Server.createWebServer("-webPort", "8082").start();
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }

        try (Connection conn = DriverManager.getConnection(DB_URL, USER, PASSWORD)) {
            System.out.println("Connected to H2 database");

            try {
                DatabaseMetaData meta = conn.getMetaData();
                ResultSet rs = meta.getTables(null, null, "PREFECTURE", null);
                if (!rs.next()) {
                    createTables(conn);
                    insertSampleData(conn);
                }
                queryData(conn);
                testDAO(conn);
            } catch (SQLException e) {
                System.out.println("Database says no: " + e.getMessage());
            }

            System.out.println("\nH2 Console: http://localhost:8082");
            System.out.println("JDBC URL: " + DB_URL);

        } catch (SQLException e) {
            e.printStackTrace();
        }
    }

    private static void testDAO(Connection conn) {
        System.out.println("\n=== DAO TESTING ===");

        // Test Prefecture DAO
        PrefectureDAO prefDAO = new PrefectureDAO(conn);

        System.out.println("All Prefectures:");
        List<Prefecture> allPrefectures = prefDAO.getAll();
        allPrefectures.forEach(System.out::println);

        System.out.println("\nGetting Prefecture with ID 1:");
        Prefecture prefecture = prefDAO.get(1);
        System.out.println(prefecture);

        // Test School DAO
        SchoolDAO schoolDAO = new SchoolDAO(conn);

        System.out.println("\nAll Schools:");
        List<School> allSchools = schoolDAO.getAll();
        allSchools.forEach(System.out::println);

        // Test adding new prefecture
        System.out.println("\nAdding new prefecture:");
        Prefecture newPrefecture = new Prefecture(0, "Hokkaido", 5200000, 83424.0, 1869);
        prefDAO.include(newPrefecture);

        System.out.println("Updated prefecture list:");
        prefDAO.getAll().forEach(System.out::println);
    }

    // ... rest of your existing methods (createTables, insertSampleData, queryData) remain the same

    private static void createTables(Connection conn) throws SQLException {
        Statement stmt = conn.createStatement();

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

        // Schools table
        stmt.execute("""
            CREATE TABLE IF NOT EXISTS school (
                id INT PRIMARY KEY AUTO_INCREMENT,
                name VARCHAR(150) NOT NULL,
                type VARCHAR(50) NOT NULL,
                students INT,
                address VARCHAR(200),
                prefecture_id INT,
                FOREIGN KEY (prefecture_id) REFERENCES prefecture(id)
            )
        """);

        // Shops table
        stmt.execute("""
            CREATE TABLE IF NOT EXISTS shop (
                id INT PRIMARY KEY AUTO_INCREMENT,
                name VARCHAR(100) NOT NULL,
                category VARCHAR(50) NOT NULL,
                employees INT,
                annual_revenue DECIMAL(15,2),
                address VARCHAR(200),
                prefecture_id INT,
                FOREIGN KEY (prefecture_id) REFERENCES prefecture(id)
            )
        """);

        // Houses table
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

        // Industries table
        stmt.execute("""
            CREATE TABLE IF NOT EXISTS industry (
                id INT PRIMARY KEY AUTO_INCREMENT,
                name VARCHAR(150) NOT NULL,
                sector VARCHAR(100) NOT NULL,
                employees INT,
                annual_output DECIMAL(15,2),
                founded_year INT,
                address VARCHAR(200),
                prefecture_id INT,
                FOREIGN KEY (prefecture_id) REFERENCES prefecture(id)
            )
        """);

        System.out.println("Tables created successfully");
    }

    private static void insertSampleData(Connection conn) throws SQLException {
        // Insert prefectures
        String prefectureSQL = "INSERT INTO prefecture (name, population, area_km2, established_year) VALUES (?, ?, ?, ?)";
        try (PreparedStatement pstmt = conn.prepareStatement(prefectureSQL)) {
            pstmt.setString(1, "Tokyo");
            pstmt.setInt(2, 14000000);
            pstmt.setDouble(3, 2194.07);
            pstmt.setInt(4, 1943);
            pstmt.executeUpdate();

            pstmt.setString(1, "Osaka");
            pstmt.setInt(2, 8800000);
            pstmt.setDouble(3, 1905.32);
            pstmt.setInt(4, 1887);
            pstmt.executeUpdate();

            pstmt.setString(1, "Kyoto");
            pstmt.setInt(2, 2600000);
            pstmt.setDouble(3, 4612.19);
            pstmt.setInt(4, 1868);
            pstmt.executeUpdate();
        }

        // Insert schools
        String schoolSQL = "INSERT INTO school (name, type, students, address, prefecture_id) VALUES (?, ?, ?, ?, ?)";
        try (PreparedStatement pstmt = conn.prepareStatement(schoolSQL)) {
            pstmt.setString(1, "University of Tokyo");
            pstmt.setString(2, "University");
            pstmt.setInt(3, 28000);
            pstmt.setString(4, "Hongo, Bunkyo City");
            pstmt.setInt(5, 1);
            pstmt.executeUpdate();

            pstmt.setString(1, "Shibuya High School");
            pstmt.setString(2, "High School");
            pstmt.setInt(3, 1200);
            pstmt.setString(4, "Shibuya District");
            pstmt.setInt(5, 1);
            pstmt.executeUpdate();

            pstmt.setString(1, "Osaka University");
            pstmt.setString(2, "University");
            pstmt.setInt(3, 23000);
            pstmt.setString(4, "Suita Campus");
            pstmt.setInt(5, 2);
            pstmt.executeUpdate();
        }

        // Insert shops
        String shopSQL = "INSERT INTO shop (name, category, employees, annual_revenue, address, prefecture_id) VALUES (?, ?, ?, ?, ?, ?)";
        try (PreparedStatement pstmt = conn.prepareStatement(shopSQL)) {
            pstmt.setString(1, "Family Mart Shibuya");
            pstmt.setString(2, "Convenience Store");
            pstmt.setInt(3, 15);
            pstmt.setBigDecimal(4, new java.math.BigDecimal("2500000.00"));
            pstmt.setString(5, "Shibuya Crossing");
            pstmt.setInt(6, 1);
            pstmt.executeUpdate();

            pstmt.setString(1, "Don Quijote Osaka");
            pstmt.setString(2, "Department Store");
            pstmt.setInt(3, 45);
            pstmt.setBigDecimal(4, new java.math.BigDecimal("12000000.00"));
            pstmt.setString(5, "Dotonbori District");
            pstmt.setInt(6, 2);
            pstmt.executeUpdate();

            pstmt.setString(1, "Kyoto Traditional Crafts");
            pstmt.setString(2, "Specialty Store");
            pstmt.setInt(3, 8);
            pstmt.setBigDecimal(4, new java.math.BigDecimal("800000.00"));
            pstmt.setString(5, "Gion District");
            pstmt.setInt(6, 3);
            pstmt.executeUpdate();
        }

        // Insert houses
        String houseSQL = "INSERT INTO house (address, type, bedrooms, bathrooms, price, residents, prefecture_id) VALUES (?, ?, ?, ?, ?, ?, ?)";
        try (PreparedStatement pstmt = conn.prepareStatement(houseSQL)) {
            pstmt.setString(1, "123 Harajuku Street");
            pstmt.setString(2, "Apartment");
            pstmt.setInt(3, 2);
            pstmt.setInt(4, 1);
            pstmt.setBigDecimal(5, new java.math.BigDecimal("450000.00"));
            pstmt.setInt(6, 3);
            pstmt.setInt(7, 1);
            pstmt.executeUpdate();

            pstmt.setString(1, "456 Namba Avenue");
            pstmt.setString(2, "Condo");
            pstmt.setInt(3, 3);
            pstmt.setInt(4, 2);
            pstmt.setBigDecimal(5, new java.math.BigDecimal("320000.00"));
            pstmt.setInt(6, 4);
            pstmt.setInt(7, 2);
            pstmt.executeUpdate();

            pstmt.setString(1, "789 Bamboo Grove Road");
            pstmt.setString(2, "Traditional House");
            pstmt.setInt(3, 4);
            pstmt.setInt(4, 2);
            pstmt.setBigDecimal(5, new java.math.BigDecimal("280000.00"));
            pstmt.setInt(6, 5);
            pstmt.setInt(7, 3);
            pstmt.executeUpdate();
        }

        // Insert industries
        String industrySQL = "INSERT INTO industry (name, sector, employees, annual_output, founded_year, address, prefecture_id) VALUES (?, ?, ?, ?, ?, ?, ?)";
        try (PreparedStatement pstmt = conn.prepareStatement(industrySQL)) {
            pstmt.setString(1, "Sony Corporation");
            pstmt.setString(2, "Electronics");
            pstmt.setInt(3, 15000);
            pstmt.setBigDecimal(4, new java.math.BigDecimal("8500000000.00"));
            pstmt.setInt(5, 1946);
            pstmt.setString(6, "Minato District");
            pstmt.setInt(7, 1);
            pstmt.executeUpdate();

            pstmt.setString(1, "Panasonic Factory");
            pstmt.setString(2, "Manufacturing");
            pstmt.setInt(3, 8500);
            pstmt.setBigDecimal(4, new java.math.BigDecimal("5200000000.00"));
            pstmt.setInt(5, 1918);
            pstmt.setString(6, "Kadoma Industrial Zone");
            pstmt.setInt(7, 2);
            pstmt.executeUpdate();

            pstmt.setString(1, "Nintendo Kyoto");
            pstmt.setString(2, "Gaming");
            pstmt.setInt(3, 3200);
            pstmt.setBigDecimal(4, new java.math.BigDecimal("1600000000.00"));
            pstmt.setInt(5, 1889);
            pstmt.setString(6, "Minami District");
            pstmt.setInt(7, 3);
            pstmt.executeUpdate();
        }

        System.out.println("Sample data inserted successfully");
    }

    private static void queryData(Connection conn) throws SQLException {
        System.out.println("\n=== CITY DATABASE SUMMARY ===");
        ResultSet rs = conn.getMetaData().getTables(null, null, null, new String[]{"TABLE"});
        while (rs.next()) {
            String tableName = rs.getString("TABLE_NAME");
            System.out.println("📋 " + tableName);
        }

        // Fixed prefecture query with proper resource management
        try (Statement stmt = conn.createStatement();
             ResultSet rsp = stmt.executeQuery("SELECT * FROM prefecture")) {

            List<Prefecture> prefectures = new ArrayList<>();
            while (rsp.next()) {
                Prefecture prefecture = new Prefecture(
                        rsp.getInt("id"),
                        rsp.getString("name"),
                        rsp.getInt("population"),
                        rsp.getDouble("area_km2"),
                        rsp.getInt("established_year")
                );
                prefectures.add(prefecture);
            }
            System.out.println("\nPrefecture Objects:");
            prefectures.forEach(System.out::println);
        }

        // Prefecture summary
        String prefectureQuery = "SELECT name, population, area_km2 FROM prefecture ORDER BY population DESC";
        try (Statement stmt = conn.createStatement();
             ResultSet rsQuery = stmt.executeQuery(prefectureQuery)) {

            System.out.println("\nPREFECTURES:");
            while (rsQuery.next()) {
                System.out.printf("%-15s | Pop: %,d | Area: %.2f km²%n",
                        rsQuery.getString("name"),
                        rsQuery.getInt("population"),
                        rsQuery.getDouble("area_km2"));
            }
        }

        // Schools by prefecture
        String schoolQuery = """
            SELECT p.name as prefecture, s.name as school, s.type, s.students 
            FROM school s 
            JOIN prefecture p ON s.prefecture_id = p.id 
            ORDER BY p.name, s.students DESC
        """;

        try (Statement stmt = conn.createStatement();
             ResultSet rsSchool = stmt.executeQuery(schoolQuery)) {

            System.out.println("\nSCHOOLS BY PREFECTURE:");
            while (rsSchool.next()) {
                System.out.printf("%-10s | %-25s | %-12s | %,d students%n",
                        rsSchool.getString("prefecture"),
                        rsSchool.getString("school"),
                        rsSchool.getString("type"),
                        rsSchool.getInt("students"));
            }
        }

        // Industry output by prefecture
        String industryQuery = """
            SELECT p.name as prefecture, 
                   COUNT(i.id) as industries,
                   SUM(i.employees) as total_employees,
                   SUM(i.annual_output) as total_output
            FROM industry i 
            JOIN prefecture p ON i.prefecture_id = p.id 
            GROUP BY p.name 
            ORDER BY total_output DESC
        """;

        try (Statement stmt = conn.createStatement();
             ResultSet rsIndustry = stmt.executeQuery(industryQuery)) {

            System.out.println("\nINDUSTRY BY PREFECTURE:");
            while (rsIndustry.next()) {
                System.out.printf("%-10s | %d industries | %,d employees | $%.2f billion%n",
                        rsIndustry.getString("prefecture"),
                        rsIndustry.getInt("industries"),
                        rsIndustry.getInt("total_employees"),
                        rsIndustry.getBigDecimal("total_output").doubleValue() / 1_000_000_000);
            }
        }

        // Count, sum and avg - fixed with proper resource management
        try (PreparedStatement stmt = conn.prepareStatement("SELECT COUNT(*) as count, SUM(employees) as sum, AVG(employees) as avg FROM industry");
             ResultSet rsStats = stmt.executeQuery()) {

            System.out.println("\nINDUSTRY STATISTICS:");
            while (rsStats.next()) {
                System.out.printf("Total Industries: %d%n", rsStats.getInt("count"));
                System.out.printf("Total Employees: %,d%n", rsStats.getInt("sum"));
                System.out.printf("Average Employees: %.2f%n", rsStats.getDouble("avg"));
            }
        }
    }
}