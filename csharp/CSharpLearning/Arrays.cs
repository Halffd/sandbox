using System;
using System.Collections.Generic;

namespace CSharpLearning
{
    public static class Arrays
    {
        public static void DemonstrateListOperations()
        {
            // First example: List of names
            Console.WriteLine("List of names:");
            List<string> names = new List<string> { "Alice", "Bob", "Charlie", "David" };
            
            foreach (string name in names)
            {
                Console.WriteLine(name);
            }

            // Sort the list
            names.Sort();
            Console.WriteLine("\nAfter sorting:");
            foreach (string name in names)
            {
                Console.WriteLine(name);
            }

            // Second example: List manipulation
            Console.WriteLine("\nList manipulation:");
            List<string> items = new List<string> { "Apple", "Banana", "Cherry" };
            
            // Add an item
            items.Add("Date");
            
            // Remove an item
            items.Remove("Banana");
            
            // Insert at position
            items.Insert(1, "Blueberry");
            
            // Display the modified list
            foreach (string item in items)
            {
                Console.WriteLine(item);
            }

            string?[] students = new string?[5];
            students[3] = "Alice";
            students[1] = "Bob";
            students[2] = "Charlie";
            students[0] = "David";
            students[4] = "Eve";
            //Delete last
            students[4] = null;
            
            // Sort with null handling
            Array.Sort(students, (a, b) => 
            {
                if (a == null && b == null) return 0;
                if (a == null) return 1;  // nulls last
                if (b == null) return -1;  // nulls last
                return string.Compare(a, b, StringComparison.Ordinal);
            });
            
            foreach (var student in students)
            {
                Console.WriteLine(student ?? "[null]");
            }
        }
    }
}
