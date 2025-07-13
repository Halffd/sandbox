import java.util.*;
import java.awt.*;
import javax.swing.*;
import java.awt.event.*;
import java.util.Timer;
import java.util.TimerTask;

public class JavaIslandSimulator extends JFrame {
    
    private static final int WIDTH = 800;
    private static final int HEIGHT = 600;
    private Island island;
    private JPanel statsPanel;
    private JLabel coffeeCountLabel;
    private JLabel populationLabel;
    private JLabel timeLabel;
    private int day = 1;
    
    public JavaIslandSimulator() {
        super("Java Island Simulator - Powered by Java Coffee");
        setSize(WIDTH, HEIGHT);
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setLayout(new BorderLayout());
        
        // Initialize island
        island = new Island(60, 40);
        
        // Create island view panel
        IslandPanel islandPanel = new IslandPanel(island);
        add(islandPanel, BorderLayout.CENTER);
        
        // Create stats panel
        createStatsPanel();
        add(statsPanel, BorderLayout.NORTH);
        
        // Create controls panel
        createControlsPanel();
        
        // Set up simulation timer
        Timer timer = new Timer();
        timer.scheduleAtFixedRate(new TimerTask() {
            @Override
            public void run() {
                island.update();
                updateStats();
                islandPanel.repaint();
                day++;
            }
        }, 1000, 1000);
        
        setVisible(true);
    }
    
    private void createStatsPanel() {
        statsPanel = new JPanel();
        statsPanel.setLayout(new FlowLayout(FlowLayout.LEFT));
        statsPanel.setBackground(new Color(165, 128, 100)); // Coffee brown
        
        coffeeCountLabel = new JLabel("Coffee Beans: 0");
        coffeeCountLabel.setForeground(Color.WHITE);
        
        populationLabel = new JLabel("Java Developers: 0");
        populationLabel.setForeground(Color.WHITE);
        
        timeLabel = new JLabel("Day: 1");
        timeLabel.setForeground(Color.WHITE);
        
        statsPanel.add(timeLabel);
        statsPanel.add(new JSeparator(SwingConstants.VERTICAL));
        statsPanel.add(coffeeCountLabel);
        statsPanel.add(new JSeparator(SwingConstants.VERTICAL));
        statsPanel.add(populationLabel);
    }
    
    private void updateStats() {
        coffeeCountLabel.setText("Coffee Beans: " + island.getCoffeeBeans());
        populationLabel.setText("Java Developers: " + island.getDevelopers());
        timeLabel.setText("Day: " + day);
    }
    
    private void createControlsPanel() {
        JPanel controlsPanel = new JPanel();
        controlsPanel.setBackground(new Color(50, 30, 20)); // Dark coffee
        
        JButton plantButton = new JButton("Plant Coffee");
        plantButton.addActionListener(e -> island.plantCoffee(10));
        
        JButton hireButton = new JButton("Hire Java Developer");
        hireButton.addActionListener(e -> island.hireDeveloper());
        
        JButton brewButton = new JButton("Brew Coffee");
        brewButton.addActionListener(e -> island.brewCoffee());
        
        controlsPanel.add(plantButton);
        controlsPanel.add(hireButton);
        controlsPanel.add(brewButton);
        
        add(controlsPanel, BorderLayout.SOUTH);
    }
    
    public static void main(String[] args) {
        try {
            UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
        } catch (Exception e) {
            e.printStackTrace();
        }
        
        SwingUtilities.invokeLater(() -> new JavaIslandSimulator());
    }
    
    // Inner class for the island model
    class Island {
        private Tile[][] grid;
        private int width, height;
        private int coffeeBeans = 100;
        private int developers = 5;
        private Random random = new Random();
        
        public Island(int width, int height) {
            this.width = width;
            this.height = height;
            grid = new Tile[width][height];
            
            // Generate island terrain
            generateTerrain();
            
            // Place initial coffee plants
            for (int i = 0; i < 20; i++) {
                int x = random.nextInt(width);
                int y = random.nextInt(height);
                if (grid[x][y].type == TileType.GRASS) {
                    grid[x][y].type = TileType.COFFEE_PLANT;
                    grid[x][y].growth = random.nextInt(5);
                }
            }
        }
        
        private void generateTerrain() {
            // Generate base ocean
            for (int x = 0; x < width; x++) {
                for (int y = 0; y < height; y++) {
                    grid[x][y] = new Tile(TileType.WATER);
                }
            }
            
            // Generate island using simplex-like noise
            for (int x = 0; x < width; x++) {
                for (int y = 0; y < height; y++) {
                    double distance = Math.sqrt(Math.pow(x - width/2, 2) + Math.pow(y - height/2, 2));
                    double islandSize = Math.min(width, height) * 0.4;
                    
                    if (distance < islandSize) {
                        grid[x][y].type = TileType.GRASS;
                        
                        // Add some beaches
                        if (distance > islandSize - 3) {
                            grid[x][y].type = TileType.SAND;
                        }
                        
                        // Add some random features
                        if (random.nextDouble() < 0.05) {
                            grid[x][y].type = TileType.ROCK;
                        }
                        if (random.nextDouble() < 0.2) {
                            grid[x][y].type = TileType.TREE;
                        }
                    }
                }
            }
            
            // Add a Java coffee shop in the center
            int centerX = width / 2;
            int centerY = height / 2;
            grid[centerX][centerY].type = TileType.COFFEE_SHOP;
            
            // Clear area around coffee shop
            for (int x = centerX - 2; x <= centerX + 2; x++) {
                for (int y = centerY - 2; y <= centerY + 2; y++) {
                    if (x >= 0 && x < width && y >= 0 && y < height) {
                        if (grid[x][y].type != TileType.COFFEE_SHOP) {
                            grid[x][y].type = TileType.GRASS;
                        }
                    }
                }
            }
        }
        
        public void update() {
            // Grow coffee
            for (int x = 0; x < width; x++) {
                for (int y = 0; y < height; y++) {
                    if (grid[x][y].type == TileType.COFFEE_PLANT) {
                        grid[x][y].growth++;
                        
                        // Harvest mature plants
                        if (grid[x][y].growth >= 5) {
                            int yield = 2 + random.nextInt(4);
                            coffeeBeans += yield;
                            grid[x][y].growth = 0;
                        }
                    }
                }
            }
            
            // Developers automatically harvest some coffee
            coffeeBeans += developers / 2;
            
            // Developers need coffee to stay productive
            int coffeeNeeded = developers;
            if (coffeeBeans >= coffeeNeeded) {
                coffeeBeans -= coffeeNeeded;
            } else {
                // Not enough coffee! Some developers leave
                int leaving = Math.min(developers, (coffeeNeeded - coffeeBeans) / 2 + 1);
                developers -= leaving;
                coffeeBeans = 0;
            }
            
            // Random events
            if (random.nextDouble() < 0.1) {
                // New developer arrives
                if (developers > 0 && coffeeBeans > 20) {
                    developers++;
                }
            }
        }
        
        public void plantCoffee(int amount) {
            for (int i = 0; i < amount; i++) {
                int x = random.nextInt(width);
                int y = random.nextInt(height);
                if (grid[x][y].type == TileType.GRASS) {
                    grid[x][y].type = TileType.COFFEE_PLANT;
                    grid[x][y].growth = 0;
                }
            }
        }
        
        public void hireDeveloper() {
            if (coffeeBeans >= 50) {
                coffeeBeans -= 50;
                developers++;
            }
        }
        
        public void brewCoffee() {
            if (coffeeBeans >= 10) {
                // Brewing coffee boosts productivity
                coffeeBeans -= 10;
                
                // Find all coffee plants and boost their growth
                for (int x = 0; x < width; x++) {
                    for (int y = 0; y < height; y++) {
                        if (grid[x][y].type == TileType.COFFEE_PLANT) {
                            grid[x][y].growth += 1;
                        }
                    }
                }
            }
        }
        
        public int getCoffeeBeans() {
            return coffeeBeans;
        }
        
        public int getDevelopers() {
            return developers;
        }
    }
    
    enum TileType {
        WATER, GRASS, SAND, TREE, ROCK, COFFEE_PLANT, COFFEE_SHOP
    }
    
    class Tile {
        TileType type;
        int growth; // For coffee plants
        
        public Tile(TileType type) {
            this.type = type;
            this.growth = 0;
        }
    }
    
    // Island rendering panel
    class IslandPanel extends JPanel {
        private Island island;
        private Color WATER_COLOR = new Color(66, 135, 245);
        private Color GRASS_COLOR = new Color(96, 171, 23);
        private Color SAND_COLOR = new Color(237, 201, 175);
        private Color TREE_COLOR = new Color(53, 94, 59);
        private Color ROCK_COLOR = new Color(128, 128, 128);
        private Color COFFEE_PLANT_COLOR = new Color(139, 69, 19);
        private Color COFFEE_SHOP_COLOR = new Color(165, 42, 42);
        
        public IslandPanel(Island island) {
            this.island = island;
            setBackground(WATER_COLOR);
        }
        
        @Override
        protected void paintComponent(Graphics g) {
            super.paintComponent(g);
            
            int tileWidth = getWidth() / island.width;
            int tileHeight = getHeight() / island.height;
            
            for (int x = 0; x < island.width; x++) {
                for (int y = 0; y < island.height; y++) {
                    int pixelX = x * tileWidth;
                    int pixelY = y * tileHeight;
                    
                    Tile tile = island.grid[x][y];
                    
                    switch (tile.type) {
                        case WATER:
                            g.setColor(WATER_COLOR);
                            break;
                        case GRASS:
                            g.setColor(GRASS_COLOR);
                            break;
                        case SAND:
                            g.setColor(SAND_COLOR);
                            break;
                        case TREE:
                            g.setColor(TREE_COLOR);
                            break;
                        case ROCK:
                            g.setColor(ROCK_COLOR);
                            break;
                        case COFFEE_PLANT:
                            // Coffee plants change color as they grow
                            int greenAmount = Math.min(255, 69 + (tile.growth * 20));
                            g.setColor(new Color(139, greenAmount, 19));
                            break;
                        case COFFEE_SHOP:
                            g.setColor(COFFEE_SHOP_COLOR);
                            break;
                    }
                    
                    g.fillRect(pixelX, pixelY, tileWidth, tileHeight);
                    
                    // Draw growth indicator for coffee plants
                    if (tile.type == TileType.COFFEE_PLANT) {
                        g.setColor(new Color(0, 0, 0, 100));
                        int growthHeight = (int)(tileHeight * (tile.growth / 5.0));
                        g.fillRect(pixelX, pixelY + tileHeight - growthHeight, 
                                   tileWidth, growthHeight);
                    }
                    
                    // Draw grid lines
                    g.setColor(new Color(0, 0, 0, 30));
                    g.drawRect(pixelX, pixelY, tileWidth, tileHeight);
                }
            }
            
            // Draw developers
            g.setColor(Color.WHITE);
            for (int i = 0; i < island.getDevelopers(); i++) {
                int devX = 100 + (i % 10) * 15;
                int devY = 100 + (i / 10) * 15;
                g.fillOval(devX, devY, 10, 10);
            }
        }
    }
}