using System.Numerics;
using System.Collections.Generic;
using System.Diagnostics;
using System.Runtime.InteropServices;
using CSharpLearning;
/// <summary>
/// Main program class demonstrating various C# features and concepts.
/// </summary>
class Program
{
    /// <summary>
    /// Entry point of the application.
    /// </summary>
    /// <param name="args">Command line arguments</param>
    static void Main(string[] args)
    {
        dictionary();
        tuple();
        anonymous();
        Bookshelf.Run();
        Pancake.Run();
        if (args.Length >= 0 || args[0] == "quit")
        {
            Environment.Exit(0);
        }
        Scraper.Run("https://example.com"); // TODO: Replace with actual URL
        Calc();
        DemoAsync();
        GameSystem.Run();
        DemoNullables();
        DemoForeach();
        Linq.Run(args);
        Calculator.Start();
        Arrays.DemonstrateListOperations();
        CalculateStudents();
        DemoLists();
        // Early exit if requested
        Diamond d = new Diamond();
        d.DoWork();
        Diamond2.Run();
        DemoConsoleOutput();
        DemoThreading();
        DemoRandomNumbers();
        DemoMathOperations();
        DemoTypeConversion();
        PlaySound();
        
        // Interactive part
        InteractiveGreeting();
    }
    public static void dictionary()
    {
        Dictionary<string, string> dictionary = new Dictionary<string, string>();
        dictionary.Add("run", "the action");
        dictionary.Add("quit", "quit the program");
        dictionary.Add("help", "show this help");
        foreach (var item in dictionary)
        {
            Console.WriteLine($"{item.Key}: {item.Value}");
        }
        var objects = new Dictionary<string, object>();
        objects.Add("run", "the action");
        objects.Add("quit", -1);
        objects.Add("help", true);
        foreach (var item in objects)
        {
            Console.WriteLine($"{item.Key}: {item.Value}");
        }
    }
    public static void tuple()
    {
        //tuples
        var comic = (Title:"The Comic", Author:"The Author", Year:2022, Chapters:10, Volume:1);
        Console.WriteLine(comic.Title);
        Console.WriteLine(comic.Author);
        Console.WriteLine(comic.Year);
        Console.WriteLine(comic.Chapters);
        Console.WriteLine(comic.Volume);
    }
    public static void anonymous()
    {
        //anonymous type
        var person = new { Name = "John", Age = 30 };
        Console.WriteLine(person.Name);
        Console.WriteLine(person.Age);
        // Using a named delegate instead of lambda in anonymous type
        Action greet = () => Console.WriteLine("Hello from delegate");
        greet();
        //anonymous method
        Action action = () => Console.WriteLine("Hello");
        action();
    }
    public static void Calc()
    {
    try
    {
        Console.WriteLine("Enter a operation e.g. (1 + 4) / 2 * 5");
        string operation = Console.ReadLine();
        
        // Actually evaluate the expression
        var result = EvaluateExpression(operation);
        Console.WriteLine($"Result: {result}");
    }
    catch (FormatException)
    {
        Console.WriteLine("Invalid input format");
    }
    catch (DivideByZeroException)
    {
        Console.WriteLine("Cannot divide by zero");
    }
    catch (OverflowException)
    {
        Console.WriteLine("Number too large");
    }
    catch (ArgumentException e)
    {
        Console.WriteLine($"Error: {e.Message}");
    }
    catch (Exception e)
    {
        Console.WriteLine($"Error: {e.Message}");
    }
}
static double EvaluateExpression(string? expression)
{
    if (string.IsNullOrWhiteSpace(expression))
    {
        throw new ArgumentException("Expression cannot be null or empty", nameof(expression));
    }
    
    // Remove any whitespace from the expression
    expression = expression.Replace(" ", "");
    
    var table = new System.Data.DataTable();
    var result = table.Compute(expression, "");
    
    var doubleResult = Convert.ToDouble(result);
    
    // Check for infinity or NaN
    if (double.IsInfinity(doubleResult) || double.IsNaN(doubleResult))
    {
        throw new DivideByZeroException("Division by zero or undefined result");
    }
    
    return doubleResult;
}
    public async static void DemoAsync()
    {
        Console.WriteLine("Hello");
        await Task.Delay(1000);
        Console.WriteLine("World");
        /// 100ms async interval for 15s
        // on new thread
        Task task = Task.Run(() =>
        {
            for (int i = 0; i < 15; i++)
            {
                Thread.Sleep(100);
                Console.WriteLine(i);
            }
        });
        //wait for task
        await task;
        // race condition
        int i = 0;
        Task task2 = Task.Run(() =>
        {
            for (int j = 0; j < 15; j++)
            {
                Thread.Sleep(100);
                i++;
            }
        });
        await task2;
        Console.WriteLine(i);
    }
    public static void DemoNullables()
    {
        #nullable enable
        int? nullableInt = null;
        string? nullableString = null;
        bool? nullableBoolean = null;
        double? nullableDouble = null;

        Object? nullableObject = null;

        Console.WriteLine(nullableInt.HasValue ? nullableInt.Value : "null");
        Console.WriteLine(nullableString);
        Console.WriteLine(nullableBoolean.HasValue ? nullableBoolean.Value : "null");
        Console.WriteLine(nullableDouble.HasValue ? nullableDouble.Value : "null");
        Console.WriteLine(nullableObject);
        #nullable disable
    }
    public static void DemoForeach()
    {
        List<int> numbers = new List<int> { 1, 2, 3, 4, 5 };
        foreach (int number in numbers)
        {
            Console.WriteLine(number);
        }
        int[] numbers2 = { 1, 2, 3, 4, 5 };
        foreach (int number in numbers2)
        {
            Console.WriteLine(number);
        }
        bool[] booleans = { true, false, true };
        foreach (bool boolean in booleans)
        {
            Console.WriteLine(boolean);
        }
        List<Object> objects = new List<Object> { 1, 2, 3, 4, 5 };
        objects.Add("Hello");
        objects.Add(6);
        foreach (Object obj in objects)
        {
            Console.WriteLine(obj);
        }
    }   
    public static void CalculateStudents()
    {
        Console.Write("How many students are in your class: ");
        string input = Console.ReadLine();
        
        if (!int.TryParse(input, out int studentCount) || studentCount <= 0)
        {
            Console.WriteLine("Invalid number, using 5 students instead.");
            studentCount = 5;
        }

        Console.WriteLine("Can you name them: ");
        
        string[] students = new string[studentCount]; // Use actual count

        for (int i = 0; i < students.Length; i++)
        {
            Console.Write($"Student {i + 1}: ");
            students[i] = Console.ReadLine();
        }

        Console.WriteLine("---------------");
        Console.WriteLine("Students in alphabetical order:");

        Array.Sort(students);

        for (int i = 0; i < students.Length; i++)
        {
            Console.WriteLine($"{i + 1}. {students[i]}");
        }

        Console.ReadKey(); // wait before closing
    }
    private static void DemoLists()
    {
        var list = new List<int>();
        list.Add(1);
        list.Add(2);
        list.Add(3);
        list.Add(4);
        list.Add(5);
        list.Add(6);
        list.Add(7);
        list.Add(8);
        list.Add(9);
        list.Add(10);
        
        Console.WriteLine("List: " + string.Join(", ", list));
        var myList = list.Where(x => x > 5).Select(x => x * 2);
        Console.WriteLine("MyList: " + string.Join(", ", myList));
    }
    /// <summary>
    /// Demonstrates basic console output and formatting.
    /// </summary>
    private static void DemoConsoleOutput()
    {
        Console.BackgroundColor = ConsoleColor.Blue;
        Console.ForegroundColor = ConsoleColor.Yellow;
        Console.WriteLine("DOES THIS WORK?");
        Thread.Sleep(1000);
        
        Console.ResetColor();
        Console.BackgroundColor = ConsoleColor.Red;
        Console.ForegroundColor = ConsoleColor.White;
        Console.WriteLine("DOES THIS WORK?");
        Thread.Sleep(1000);
        Console.ResetColor();
        
        // Set console appearance
        Console.Title = "Skynet Terminal";
        Console.ForegroundColor = ConsoleColor.Green;
        Console.BackgroundColor = ConsoleColor.Black;
        
        // Set window size and position
        if (RuntimeInformation.IsOSPlatform(OSPlatform.Windows))
        {
            Console.SetWindowSize(80, 25); // Width: 80 chars, Height: 25 lines
            Console.SetWindowPosition(200, 100); // X: 200px, Y: 100px from top-left
        }
    }

    /// <summary>
    /// Demonstrates basic threading operations.
    /// </summary>
    private static void DemoThreading()
    {
        int i = 0;
        var threads = new List<Thread>();
        object lockObject = new object();

        for (int index = 0; index < 1000; index++)
        {
            var thread = new Thread(() =>
            {
                Console.WriteLine("Hello from a thread!");
                
                lock (lockObject)
                {
                    i = i + 1 + i * 2;
                }
            });
            
            threads.Add(thread);
            thread.Start();
        }

        foreach (var thread in threads)
        {
            thread.Join();
        }

        Console.WriteLine($"Final value of i: {i}");
        
        for (int j = 0; j < 3; j++)
        {
            Thread.Sleep(1000);
            Console.WriteLine("Thread.Sleep(1000);");
        }
    }

    /// <summary>
    /// Demonstrates random number generation.
    /// </summary>
    private static void DemoRandomNumbers()
    {
        var random = new Random();
        
        Console.WriteLine("\nRandom Numbers:");
        Console.WriteLine($"Next(): {random.Next()}");
        
        for (int i = 0; i < 3; i++)
        {
            Console.WriteLine($"Next(1, 10): {random.Next(1, 10)}");
        }
        
        Console.WriteLine($"NextDouble(): {random.NextDouble():F2}");
    }

    /// <summary>
    /// Demonstrates various mathematical operations.
    /// </summary>
    private static void DemoMathOperations()
    {
        Console.WriteLine("\nMath Operations:");
        
        // BigInteger demo
        BigInteger bigInt = 1;
        Console.WriteLine($"BigInteger: {bigInt} (Type: {bigInt.GetType()})");
        
        bigInt = BigInteger.Pow(2, 64);
        Console.WriteLine($"2^64 = {bigInt} (Type: {bigInt.GetType()})");
        
        // Math class operations
        Console.WriteLine($"Math.Pow(2, 64): {Math.Pow(2, 64)}");
        Console.WriteLine($"Math.Ceiling(2.5): {Math.Ceiling(2.5)}");
        Console.WriteLine($"Math.Floor(2.5): {Math.Floor(2.5)}");
        Console.WriteLine($"Math.Sqrt(2): {Math.Sqrt(2):F15}");
        Console.WriteLine($"Math.Round(2.5): {Math.Round(2.5)}");
        Console.WriteLine($"Math.Round(2.51): {Math.Round(2.51)}");
        Console.WriteLine($"Math.Round(2.49): {Math.Round(2.49)}");
    }

    /// <summary>
    /// Demonstrates type conversion in C#.
    /// </summary>
    private static void DemoTypeConversion()
    {
        Console.WriteLine("\nType Conversion:");
        
        double pi = Math.PI;
        int piInt = Convert.ToInt32(pi);
        double piIntDouble = Convert.ToDouble(piInt) + Math.PI;
        string piString = pi.ToString();
        
        Console.WriteLine($"PI: {pi} (Type: {pi.GetType()})");
        Console.WriteLine($"PI as Int32: {piInt} (Type: {piInt.GetType()})");
        Console.WriteLine($"PI as String: {piString} (Type: {piString.GetType()})");
        
        // Character and boolean conversion
        char c = 'a';
        uint u = Convert.ToUInt32(c);
        bool boolean = Convert.ToBoolean("true");
        
        Console.WriteLine($"Char 'a' to UInt32: {u}");
        Console.WriteLine($"String \"true\" to boolean: {boolean}");
    }

    /// <summary>
    /// Plays system sounds based on the operating system.
    /// </summary>
    private static void PlaySound()
    {
        if (OperatingSystem.IsWindows())
        {
            Console.Beep();
        }
        else if (OperatingSystem.IsLinux())
        {
            // Linux beep via shell (limited to 5 beeps for demo)
            for (int freq = 1; freq <= 5; freq++)
            {
                Process.Start("speaker-test", $"-t sine -f {freq * 100} -l 1 -s 1");
                Thread.Sleep(500);
            }
        }
    }

    /// <summary>
    /// Interactive greeting with user input.
    /// </summary>
    private static void InteractiveGreeting()
    {
        Console.Write("\nWhat's your name? ");
        string name = Console.ReadLine() ?? "Stranger";
        Console.WriteLine($"Hello, {name}!");
        
        // Simple function demonstration
        int Add(int a, int b) => a + b;
        Console.WriteLine($"5 + 3 = {Add(5, 3)}");
    }
}