package com.half.javalearning.classes;

// Example class to demonstrate Object inheritance in Java
public class ObjectInheritanceDemo {
    public static void main(String[] args) {
        // Create two Student objects
        Student student1 = new Student("João", 20, "CS101");
        Student student2 = new Student("João", 20, "CS101");
        Student student3 = new Student("Maria", 22, "ENG202");

        // Using the equals() method inherited from Object
        System.out.println("student1 equals student2: " + student1.equals(student2)); // Will use our overridden method
        System.out.println("student1 equals student3: " + student1.equals(student3));

        // Using other methods from Object class
        System.out.println("\nstudent1 toString(): " + student1.toString());
        System.out.println("student1 hashCode(): " + student1.hashCode());

        // Demonstrating that any object can be assigned to Object type
        Object obj = student1; // Student is a subclass of Object
        System.out.println("\nObject reference: " + obj);
    }
}

// Student class that overrides Object methods
class Student {
    private String name;
    private int age;
    private String courseId;

    public Student(String name, int age, String courseId) {
        this.name = name;
        this.age = age;
        this.courseId = courseId;
    }

    // Override equals() method from Object class
    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || getClass() != obj.getClass()) return false;

        Student student = (Student) obj;
        return age == student.age &&
                name.equals(student.name) &&
                courseId.equals(student.courseId);
    }

    // Override toString() method from Object class
    @Override
    public String toString() {
        return "Student{name='" + name + "', age=" + age + ", courseId='" + courseId + "'}";
    }

    // Override hashCode() method from Object class
    @Override
    public int hashCode() {
        int result = name.hashCode();
        result = 31 * result + age;
        result = 31 * result + courseId.hashCode();
        return result;
    }
}
