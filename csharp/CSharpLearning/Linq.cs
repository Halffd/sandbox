using System;
using System.Collections.Generic;
using System.Linq; // Required for LINQ operations

namespace CSharpLearning
{
    public class Linq
    {
        public static void Run(string[] args)
        {
            Console.WriteLine("--- Demonstrating Functional Patterns (Func) ---");

            // Define a Func delegate named 'doubleIt'
            // It takes an integer (int) as input and returns an integer (int).
            // The lambda expression 'x => x * 2' defines the logic: take 'x' and return 'x' multiplied by 2.
            Func<int, int> doubleIt = x => x * 2;

            // Call the 'doubleIt' function with an example value (23) and print the result.
            int resultOfDoubleIt = doubleIt(23);
            Console.WriteLine($"Original value: 23");
            Console.WriteLine($"Doubled value: {resultOfDoubleIt}"); // Expected output: 46
            Console.WriteLine(); // Add an empty line for readability

            Console.WriteLine("--- Demonstrating LINQ Query ---");

            // Create a sample list of integers to perform LINQ operations on.
            List<int> numbers = new List<int> { 1, 8, 2, 9, 5, 0, 10, 3, 7, 4, 6 };

            Console.WriteLine($"Original list of numbers: {string.Join(", ", numbers)}");

            // Define a LINQ query to filter and order numbers.
            // 1. 'from num in numbers': Start by iterating over each 'num' in the 'numbers' list.
            // 2. 'where num < 3 || num > 7': Filter the numbers. Only include numbers that are
            //    less than 3 OR greater than 7.
            // 3. 'orderby num ascending': Sort the filtered numbers in ascending order.
            // 4. 'select num': Select the resulting numbers to form the final collection.
            IEnumerable<int> orderingQuery =
                from num in numbers
                where num < 3 || num > 7
                orderby num ascending
                select num;

            Console.WriteLine("Numbers less than 3 or greater than 7 (ordered ascending):");

            // Iterate through the results of the LINQ query and print each number.
            foreach (int num in orderingQuery)
            {
                Console.WriteLine(num);
            }

            Console.WriteLine("\nPress any key to exit.");
            Console.ReadKey(); // Keep the console window open until a key is pressed
        }
    }
}
