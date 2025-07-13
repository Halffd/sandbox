package com.half.javalearning.classes.liskov;
// This violates LSP - behavioral incompatibility
class Rectangle {
    protected int width, height;
    void setWidth(int w) { width = w; }
    void setHeight(int h) { height = h; }
    int getWidth() { return width; }
    int getHeight() { return height; }
    int area() { return width * height; }
    // diagonals
    int diagonal() { return (int) Math.sqrt(width * width + height * height); }
}
class RectangleCorrect {
    private final int width, height;

    public RectangleCorrect(int width, int height) {
        this.width = width;
        this.height = height;
    }

    public int getWidth() { return width; }
    public int getHeight() { return height; }
    public int area() { return width * height; }
    public int perimeter() { return 2 * (width + height); }
}

class RectangleCalculus {
    private final Rectangle rectangle;

    public RectangleCalculus(Rectangle rectangle) {
        this.rectangle = rectangle;
    }

    public double integralOfXPlusY() {
        int w = rectangle.getWidth();
        int h = rectangle.getHeight();
        return (w * w * h + w * h * h) / 2.0;
    }

    // Add more calculus methods here without polluting Rectangle
}
public class Square extends Rectangle {
    void setWidth(int w) { width = height = w; } // Changes expected behavior!
    void setHeight(int h) { width = height = h; } // Violates rectangle assumptions
    public static void main(String[] args) {
        Rectangle r = new Rectangle();
        r.setWidth(5);
        r.setHeight(10);
        System.out.println("Area: " + r.area());
        System.out.println("Diagonal: " + r.diagonal());
        Square s = new Square();
        s.setWidth(5);
        s.setHeight(10);
        System.out.println("Area: " + s.area());
        System.out.println("Diagonal: " + s.diagonal());
        // Usage:
        RectangleCalculus calc = new RectangleCalculus(s);
        System.out.println("Integral: " + calc.integralOfXPlusY());
    }
}