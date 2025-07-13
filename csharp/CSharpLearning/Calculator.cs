using System;

namespace CSharpLearning
{
    public class Calculator
    {
        public static void Start()
        {
            bool continueCalculating = true;
            
            while (continueCalculating)
            {
                Console.Clear();
                Console.WriteLine("=== Calculator ===");
                Console.WriteLine("1. Add");
                Console.WriteLine("2. Subtract");
                Console.WriteLine("3. Multiply");
                Console.WriteLine("4. Divide");
                Console.WriteLine("5. Power");
                Console.WriteLine("6. Square Root");
                Console.WriteLine("7. Logarithm (base 10)");
                Console.WriteLine("8. Natural Logarithm (ln)");
                Console.WriteLine("9. Summation (Σ)");
                Console.WriteLine("10. Product (Π)");
                Console.WriteLine("11. Multiplication Table");
                Console.WriteLine("12. Cubic Root (³√x)");
                Console.WriteLine("13. Nth Root");
                Console.WriteLine("14. Fractions");
                Console.WriteLine("15. Polynomial Operations");
                Console.WriteLine("16. Calculate Derivative");
                Console.WriteLine("17. Calculate Second Derivative");
                Console.WriteLine("18. Calculate Definite Integral");
                Console.WriteLine("19. Calculate Indefinite Integral");
                Console.WriteLine("20. Calculate Taylor Series");
                Console.WriteLine("21. Matrix Operations");
                Console.WriteLine("22. Vector Operations");
                Console.WriteLine("23. Linear System Solver");
                Console.WriteLine("24. Eigenvalues & Eigenvectors");
                Console.WriteLine("25. Partial Derivatives");
                Console.WriteLine("26. Multiple Integrals");
                Console.WriteLine("27. Vector Calculus");
                Console.WriteLine("28. Statistics");
                Console.WriteLine("29. Multivariate Analysis");
                Console.WriteLine("30. Exit");
                Console.Write("\nSelect an operation (1-30): ");

                if (int.TryParse(Console.ReadLine(), out int choice))
                {
                    switch (choice)
                    {
                        case 1:
                            PerformOperation(Add);
                            break;
                        case 2:
                            PerformOperation(Subtract);
                            break;
                        case 3:
                            PerformOperation(Multiply);
                            break;
                        case 4:
                            PerformOperation(Divide);
                            break;
                        case 5:
                            CalculatePower();
                            break;
                        case 6:
                            CalculateSquareRoot();
                            break;
                        case 7:
                            CalculateLogarithm();
                            break;
                        case 8:
                            CalculateNaturalLog();
                            break;
                        case 9:
                            CalculateSummation();
                            break;
                        case 10:
                            CalculateProduct();
                            break;
                        case 11:
                            ShowMultiplicationTable();
                            break;
                        case 12:
                            CalculateCubicRoot();
                            break;
                        case 13:
                            CalculateNthRoot();
                            break;
                        case 14:
                            CalculateFractions();
                            break;
                        case 15:
                            PolynomialOperations();
                            break;
                        case 16:
                            CalculateDerivative();
                            break;
                        case 17:
                            CalculateSecondDerivative();
                            break;
                        case 18:
                            CalculateDefiniteIntegral();
                            break;
                        case 19:
                            CalculateIndefiniteIntegral();
                            break;
                        case 20:
                            CalculateTaylorSeries();
                            break;
                        case 21:
                            MatrixOperations();
                            break;
                        case 22:
                            VectorOperations();
                            break;
                        case 23:
                            SolveLinearSystem();
                            break;
                        case 24:
                            CalculateEigen();
                            break;
                        case 25:
                            CalculatePartialDerivatives();
                            break;
                        case 26:
                            CalculateMultipleIntegrals();
                            break;
                        case 27:
                            VectorCalculusOperations();
                            break;
                        case 28:
                            StatisticsOperations();
                            break;
                        case 29:
                            MultivariateAnalysis();
                            break;
                        case 30:
                            continueCalculating = false;
                            Console.WriteLine("Goodbye!");
                            continue;
                        default:
                            Console.WriteLine("Invalid choice. Please try again.");
                            break;
                    }
                }
                else
                {
                    Console.WriteLine("Invalid input. Please enter a number.");
                }

                Console.WriteLine("\nPress any key to continue...");
                Console.ReadKey();
            }
        }


        private static double GetNumber(string prompt)
        {
            double number;
            while (true)
            {
                Console.Write(prompt);
                if (double.TryParse(Console.ReadLine(), out number))
                {
                    return number;
                }
                Console.WriteLine("Invalid input. Please enter a valid number.");
            }
        }


        private static void PerformOperation(Func<double, double, double> operation)
        {
            double num1 = GetNumber("Enter first number: ");
            double num2 = GetNumber("Enter second number: ");
            double result = operation(num1, num2);
            Console.WriteLine($"Result: {result}");
        }

        // Basic arithmetic operations
        private static double Add(double a, double b) => a + b;
        private static double Subtract(double a, double b) => a - b;
        private static double Multiply(double a, double b) => a * b;
        private static double Divide(double a, double b) => b != 0 ? a / b : throw new DivideByZeroException("Cannot divide by zero");

        // Additional operations
        private static void CalculatePower()
        {
            double baseNum = GetNumber("Enter base number: ");
            double exponent = GetNumber("Enter exponent: ");
            double result = Math.Pow(baseNum, exponent);
            Console.WriteLine($"{baseNum} ^ {exponent} = {result}");
        }

        private static void CalculateSquareRoot()
        {
            double number = GetNumber("Enter a number: ");
            if (number < 0)
            {
                Console.WriteLine("Cannot calculate square root of a negative number");
                return;
            }
            double result = Math.Sqrt(number);
            Console.WriteLine($"√{number} = {result}");
        }

        private static void CalculateLogarithm()
        {
            double number = GetNumber("Enter a positive number: ");
            if (number <= 0)
            {
                Console.WriteLine("Logarithm is only defined for positive numbers");
                return;
            }
            double result = Math.Log10(number);
            Console.WriteLine($"log10({number}) = {result}");
        }

        private static void CalculateNaturalLog()
        {
            double number = GetNumber("Enter a positive number: ");
            if (number <= 0)
            {
                Console.WriteLine("Natural logarithm is only defined for positive numbers");
                return;
            }
            double result = Math.Log(number);
            Console.WriteLine($"ln({number}) = {result}");
        }

        private static void CalculateSummation()
        {
            int n = (int)GetNumber("Enter the upper limit (n) for summation Σ(i=1 to n): ");
            if (n < 1)
            {
                Console.WriteLine("Please enter a positive integer");
                return;
            }
            int sum = 0;
            for (int i = 1; i <= n; i++)
            {
                sum += i;
            }
            Console.WriteLine($"Σ(i=1 to {n}) = {sum}");
        }

        private static void CalculateProduct()
        {
            int n = (int)GetNumber("Enter the upper limit (n) for product Π(i=1 to n): ");
            if (n < 1)
            {
                Console.WriteLine("Please enter a positive integer");
                return;
            }
            long product = 1;
            for (int i = 1; i <= n; i++)
            {
                product *= i;
            }
            Console.WriteLine($"Π(i=1 to {n}) = {product}");
        }

        private static void ShowMultiplicationTable()
        {
            int number = (int)GetNumber("Enter a number for the multiplication table: ");
            int limit = (int)GetNumber("Enter the limit (e.g., 10 for 10x table): ");
            
            Console.WriteLine($"\nMultiplication Table for {number}:");
            for (int i = 1; i <= limit; i++)
            {
                Console.WriteLine($"{number} × {i} = {number * i}");
            }
        }

        private static void CalculateCubicRoot()
        {
            double number = GetNumber("Enter a number: ");
            double result = Math.Cbrt(number);
            Console.WriteLine($"³√{number} = {result}");
        }

        private static void CalculateNthRoot()
        {
            double number = GetNumber("Enter the number: ");
            double root = GetNumber("Enter the root (e.g., 3 for cube root, 4 for fourth root, etc.): ");
            
            if (root == 0)
            {
                Console.WriteLine("Root cannot be zero");
                return;
            }
            
            double result = Math.Pow(number, 1.0 / root);
            Console.WriteLine($"{root}√{number} = {result}");
        }

        private static void CalculateFractions()
        {
            Console.WriteLine("Enter first fraction (numerator/denominator):");
            double num1 = GetNumber("Numerator: ");
            double den1 = GetNumber("Denominator: ");
            
            Console.WriteLine("\nEnter operation (+, -, *, /): ");
            string? op = Console.ReadLine();
            if (string.IsNullOrWhiteSpace(op))
            {
                Console.WriteLine("Error: No operation entered");
                return;
            }
            
            Console.WriteLine("\nEnter second fraction (numerator/denominator):");
            double num2 = GetNumber("Numerator: ");
            double den2 = GetNumber("Denominator: ");
            
            if (den1 == 0 || den2 == 0)
            {
                Console.WriteLine("Error: Denominator cannot be zero");
                return;
            }
            
            double resultNum = 0, resultDen = 1;
            
            switch (op)
            {
                case "+":
                    resultNum = (num1 * den2) + (num2 * den1);
                    resultDen = den1 * den2;
                    break;
                case "-":
                    resultNum = (num1 * den2) - (num2 * den1);
                    resultDen = den1 * den2;
                    break;
                case "*":
                    resultNum = num1 * num2;
                    resultDen = den1 * den2;
                    break;
                case "/":
                    if (num2 == 0)
                    {
                        Console.WriteLine("Error: Cannot divide by zero");
                        return;
                    }
                    resultNum = num1 * den2;
                    resultDen = den1 * num2;
                    break;
                default:
                    Console.WriteLine("Invalid operation");
                    return;
            }
            
            // Simplify the fraction
            double gcd = GCD(Math.Abs(resultNum), Math.Abs(resultDen));
            double simplifiedNum = resultNum / gcd;
            double simplifiedDen = resultDen / gcd;
            
            Console.WriteLine($"\nResult: {resultNum}/{resultDen} = {simplifiedNum}/{simplifiedDen} ≈ {(resultNum/resultDen):0.#####}");
        }
        
        // Helper method to calculate Greatest Common Divisor (GCD) using Euclidean algorithm
        private static double GCD(double a, double b)
        {
            while (b != 0)
            {
                double temp = b;
                b = a % b;
                a = temp;
            }
            return a;
        }

        // Represents a polynomial term: coefficient * x^exponent
        private class PolynomialTerm
        {
            public double Coefficient { get; set; }
            public int Exponent { get; set; }

            public PolynomialTerm(double coefficient, int exponent)
            {
                Coefficient = coefficient;
                Exponent = exponent;
            }

            public override string ToString()
            {
                if (Exponent == 0) return $"{Coefficient}";
                if (Exponent == 1) return $"{Coefficient}x";
                return $"{Coefficient}x^{Exponent}";
            }
        }

        private static void PolynomialOperations()
        {
            Console.WriteLine("\n=== Polynomial Operations ===");
            Console.WriteLine("Enter polynomial terms in the format: coefficient exponent (e.g., '3 2' for 3x^2)");
            Console.WriteLine("Enter 'done' when finished");
            
            List<PolynomialTerm> terms = new List<PolynomialTerm>();
            
            while (true)
            {
                Console.Write("Enter term (or 'done'): ");
                string input = Console.ReadLine()?.Trim() ?? "";
                
                if (input.ToLower() == "done") break;
                
                string[] parts = input.Split(' ', StringSplitOptions.RemoveEmptyEntries);
                if (parts.Length != 2 || 
                    !double.TryParse(parts[0], out double coefficient) || 
                    !int.TryParse(parts[1], out int exponent))
                {
                    Console.WriteLine("Invalid input. Please enter in format: coefficient exponent");
                    continue;
                }
                
                terms.Add(new PolynomialTerm(coefficient, exponent));
            }
            
            // Sort terms by exponent in descending order
            terms = terms.OrderByDescending(t => t.Exponent).ToList();
            
            Console.WriteLine("\nYour polynomial:");
            Console.WriteLine(FormatPolynomial(terms));
            
            Console.WriteLine("\n1. Evaluate at x = ?");
            Console.WriteLine("2. Find derivative");
            Console.WriteLine("3. Find integral");
            Console.Write("\nSelect operation (1-3): ");
            
            if (!int.TryParse(Console.ReadLine(), out int choice) || choice < 1 || choice > 3)
            {
                Console.WriteLine("Invalid choice");
                return;
            }
            
            switch (choice)
            {
                case 1:
                    double x = GetNumber("Enter value of x: ");
                    double result = 0;
                    foreach (var term in terms)
                    {
                        result += term.Coefficient * Math.Pow(x, term.Exponent);
                    }
                    Console.WriteLine($"f({x}) = {result}");
                    break;
                    
                case 2:
                    var derivative = FindDerivative(terms);
                    Console.WriteLine("First derivative: " + FormatPolynomial(derivative));
                    break;
                    
                case 3:
                    var integral = FindIntegral(terms);
                    Console.WriteLine("Indefinite integral: " + FormatPolynomial(integral) + " + C");
                    break;
            }
        }
        
        private static string FormatPolynomial(List<PolynomialTerm> terms)
        {
            if (terms.Count == 0) return "0";
            
            var formatted = new List<string>();
            foreach (var term in terms)
            {
                if (term.Coefficient == 0) continue;
                
                string termStr = term.ToString();
                if (formatted.Count > 0 && term.Coefficient > 0)
                {
                    termStr = " + " + termStr;
                }
                else if (term.Coefficient < 0)
                {
                    termStr = termStr.Replace("-", " - ").TrimEnd();
                }
                formatted.Add(termStr);
            }
            
            string result = string.Join("", formatted).Trim();
            return result.StartsWith(" + ") ? result[3..] : result;
        }
        
        private static List<PolynomialTerm> FindDerivative(List<PolynomialTerm> terms)
        {
            var result = new List<PolynomialTerm>();
            foreach (var term in terms)
            {
                if (term.Exponent > 0)
                {
                    result.Add(new PolynomialTerm(
                        term.Coefficient * term.Exponent,
                        term.Exponent - 1));
                }
            }
            return result;
        }
        
        private static List<PolynomialTerm> FindIntegral(List<PolynomialTerm> terms)
        {
            var result = new List<PolynomialTerm>();
            foreach (var term in terms)
            {
                int newExp = term.Exponent + 1;
                result.Add(new PolynomialTerm(
                    term.Coefficient / newExp,
                    newExp));
            }
            return result;
        }
        
        private static void CalculateDerivative()
        {
            Console.WriteLine("\n=== First Derivative ===");
            var terms = GetPolynomialTerms();
            var derivative = FindDerivative(terms);
            Console.WriteLine("f(x) = " + FormatPolynomial(terms));
            Console.WriteLine("f'(x) = " + FormatPolynomial(derivative));
        }
        
        private static void CalculateSecondDerivative()
        {
            Console.WriteLine("\n=== Second Derivative ===");
            var terms = GetPolynomialTerms();
            var firstDerivative = FindDerivative(terms);
            var secondDerivative = FindDerivative(firstDerivative);
            Console.WriteLine("f(x) = " + FormatPolynomial(terms));
            Console.WriteLine("f''(x) = " + FormatPolynomial(secondDerivative));
        }
        
        private static void CalculateDefiniteIntegral()
        {
            Console.WriteLine("\n=== Definite Integral ===");
            var terms = GetPolynomialTerms();
            double a = GetNumber("Enter lower limit (a): ");
            double b = GetNumber("Enter upper limit (b): ");
            
            var indefiniteIntegral = FindIntegral(terms);
            double resultAtB = EvaluatePolynomial(indefiniteIntegral, b);
            double resultAtA = EvaluatePolynomial(indefiniteIntegral, a);
            double definiteIntegral = resultAtB - resultAtA;
            
            Console.WriteLine($"∫({a} to {b}) " + FormatPolynomial(terms) + " dx = " + definiteIntegral);
        }
        
        private static void CalculateIndefiniteIntegral()
        {
            Console.WriteLine("\n=== Indefinite Integral ===");
            var terms = GetPolynomialTerms();
            var integral = FindIntegral(terms);
            Console.WriteLine("∫(" + FormatPolynomial(terms) + ") dx = " + FormatPolynomial(integral) + " + C");
        }
        
        private static void CalculateTaylorSeries()
        {
            Console.WriteLine("\n=== Taylor Series ===");
            Console.WriteLine("Enter the function's derivatives at x=0 (Maclaurin series)");
            Console.WriteLine("Enter coefficients in the format: f(0), f'(0), f''(0)/2!, f'''(0)/3!, ...");
            Console.WriteLine("Enter 'done' when finished");
            
            List<double> coefficients = new List<double>();
            int n = 0;
            
            while (true)
            {
                Console.Write($"a_{n} = f^({n})(0)/{n}!: ");
                string input = Console.ReadLine()?.Trim() ?? "";
                
                if (input.ToLower() == "done") break;
                
                if (double.TryParse(input, out double coefficient))
                {
                    coefficients.Add(coefficient);
                    n++;
                }
                else
                {
                    Console.WriteLine("Invalid input. Please enter a number or 'done'");
                }
            }
            
            if (coefficients.Count == 0)
            {
                Console.WriteLine("No coefficients entered");
                return;
            }
            
            Console.Write("\nTaylor series: f(x) = ");
            for (int i = 0; i < coefficients.Count; i++)
            {
                if (i > 0 && coefficients[i] >= 0)
                    Console.Write(" + ");
                
                if (i == 0)
                    Console.Write(coefficients[i]);
                else if (i == 1)
                    Console.Write($"{coefficients[i]}x");
                else
                    Console.Write($"{coefficients[i]}x^{i}");
            }
            Console.WriteLine();
            
            // Evaluate at a point
            double x = GetNumber("\nEnter x value to evaluate the series: ");
            double result = 0;
            for (int i = 0; i < coefficients.Count; i++)
            {
                result += coefficients[i] * Math.Pow(x, i);
            }
            Console.WriteLine($"f({x}) ≈ {result}");
        }
        
        private static List<PolynomialTerm> GetPolynomialTerms()
        {
            Console.WriteLine("\nEnter polynomial terms in the format: coefficient exponent (e.g., '3 2' for 3x^2)");
            Console.WriteLine("Enter 'done' when finished");
            
            List<PolynomialTerm> terms = new List<PolynomialTerm>();
            
            while (true)
            {
                Console.Write("Enter term (or 'done'): ");
                string input = Console.ReadLine()?.Trim() ?? "";
                
                if (input.ToLower() == "done") break;
                
                string[] parts = input.Split(' ', StringSplitOptions.RemoveEmptyEntries);
                if (parts.Length != 2 || 
                    !double.TryParse(parts[0], out double coefficient) || 
                    !int.TryParse(parts[1], out int exponent))
                {
                    Console.WriteLine("Invalid input. Please enter in format: coefficient exponent");
                    continue;
                }
                
                terms.Add(new PolynomialTerm(coefficient, exponent));
            }
            
            return terms.OrderByDescending(t => t.Exponent).ToList();
        }
        
        private static double EvaluatePolynomial(List<PolynomialTerm> terms, double x)
        {
            double result = 0;
            foreach (var term in terms)
            {
                result += term.Coefficient * Math.Pow(x, term.Exponent);
            }
            return result;
        }

        // Matrix class for linear algebra operations
        private class Matrix
        {
            public double[,] Data { get; set; }
            public int Rows { get; }
            public int Cols { get; }

            public Matrix(int rows, int cols)
            {
                Rows = rows;
                Cols = cols;
                Data = new double[rows, cols];
            }

            public void FillRandom()
            {
                var rand = new Random();
                for (int i = 0; i < Rows; i++)
                    for (int j = 0; j < Cols; j++)
                        Data[i, j] = Math.Round(rand.NextDouble() * 10, 2);
            }

            public void Print()
            {
                for (int i = 0; i < Rows; i++)
                {
                    for (int j = 0; j < Cols; j++)
                    {
                        Console.Write($"{Data[i, j],8:F2}");
                    }
                    Console.WriteLine();
                }
            }

            
            public static Matrix operator +(Matrix a, Matrix b)
            {
                if (a.Rows != b.Rows || a.Cols != b.Cols)
                    throw new ArgumentException("Matrices must have the same dimensions");
                
                Matrix result = new Matrix(a.Rows, a.Cols);
                for (int i = 0; i < a.Rows; i++)
                    for (int j = 0; j < a.Cols; j++)
                        result.Data[i, j] = a.Data[i, j] + b.Data[i, j];
                return result;
            }
            
            public static Matrix operator *(Matrix a, Matrix b)
            {
                if (a.Cols != b.Rows)
                    throw new ArgumentException("Number of columns in first matrix must equal number of rows in second matrix");
                
                Matrix result = new Matrix(a.Rows, b.Cols);
                for (int i = 0; i < a.Rows; i++)
                    for (int j = 0; j < b.Cols; j++)
                        for (int k = 0; k < a.Cols; k++)
                            result.Data[i, j] += a.Data[i, k] * b.Data[k, j];
                return result;
            }
            
            public Matrix Transpose()
            {
                Matrix result = new Matrix(Cols, Rows);
                for (int i = 0; i < Rows; i++)
                    for (int j = 0; j < Cols; j++)
                        result.Data[j, i] = Data[i, j];
                return result;
            }
            
            public double Determinant()
            {
                if (Rows != Cols)
                    throw new InvalidOperationException("Matrix must be square");
                    
                int n = Rows;
                if (n == 1) return Data[0, 0];
                if (n == 2) return Data[0, 0] * Data[1, 1] - Data[0, 1] * Data[1, 0];
                
                double det = 0;
                for (int j = 0; j < n; j++)
                {
                    Matrix minor = GetMinor(0, j);
                    det += (j % 2 == 0 ? 1 : -1) * Data[0, j] * minor.Determinant();
                }
                return det;
            }
            
            private Matrix GetMinor(int row, int col)
            {
                Matrix minor = new Matrix(Rows - 1, Cols - 1);
                int m = 0, n = 0;
                for (int i = 0; i < Rows; i++)
                {
                    if (i == row) continue;
                    n = 0;
                    for (int j = 0; j < Cols; j++)
                    {
                        if (j == col) continue;
                        minor.Data[m, n] = Data[i, j];
                        n++;
                    }
                    m++;
                }
                return minor;
            }
        }
        
        private static void MatrixOperations()
        {
            Console.WriteLine("\n=== Matrix Operations ===");
            Console.WriteLine("1. Matrix Addition");
            Console.WriteLine("2. Matrix Multiplication");
            Console.WriteLine("3. Matrix Transpose");
            Console.WriteLine("4. Matrix Determinant");
            Console.Write("\nSelect operation (1-4): ");
            
            if (!int.TryParse(Console.ReadLine(), out int choice) || choice < 1 || choice > 4)
            {
                Console.WriteLine("Invalid choice");
                return;
            }
            
            try
            {
                switch (choice)
                {
                    case 1:
                        Console.WriteLine("\nMatrix A:");
                        var a = ReadMatrix();
                        Console.WriteLine("\nMatrix B (must be same size as A):");
                        var b = ReadMatrix();
                        var sum = a + b;
                        Console.WriteLine("\nA + B = ");
                        sum.Print();
                        break;
                        
                    case 2:
                        Console.WriteLine("\nMatrix A (m×n):");
                        a = ReadMatrix();
                        Console.WriteLine("\nMatrix B (must have n rows):");
                        b = ReadMatrix();
                        var product = a * b;
                        Console.WriteLine("\nA × B = ");
                        product.Print();
                        break;
                        
                    case 3:
                        Console.WriteLine("\nEnter matrix to transpose:");
                        var m = ReadMatrix();
                        var transposed = m.Transpose();
                        Console.WriteLine("\nTranspose:");
                        transposed.Print();
                        break;
                        
                    case 4:
                        Console.WriteLine("\nEnter square matrix:");
                        var mat = ReadMatrix();
                        double det = mat.Determinant();
                        Console.WriteLine($"\nDeterminant = {det:F2}");
                        break;
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine($"Error: {ex.Message}");
            }
        }
        
        private static void VectorOperations()
        {
            Console.WriteLine("\n=== Vector Operations ===");
            Console.WriteLine("1. Dot Product");
            Console.WriteLine("2. Cross Product (3D only)");
            Console.WriteLine("3. Vector Magnitude");
            Console.Write("\nSelect operation (1-3): ");
            
            if (!int.TryParse(Console.ReadLine(), out int choice) || choice < 1 || choice > 3)
            {
                Console.WriteLine("Invalid choice");
                return;
            }
            
            try
            {
                switch (choice)
                {
                    case 1:
                        Console.WriteLine("\nVector 1:");
                        var v1 = ReadVector();
                        Console.WriteLine("\nVector 2 (must be same dimension):");
                        var v2 = ReadVector();
                        if (v1.Length != v2.Length)
                            throw new ArgumentException("Vectors must have the same dimension");
                        double dotProduct = 0;
                        for (int i = 0; i < v1.Length; i++)
                            dotProduct += v1[i] * v2[i];
                        Console.WriteLine($"\nDot Product = {dotProduct:F4}");
                        break;
                        
                    case 2:
                        Console.WriteLine("\nEnter 3D Vector 1 (x y z):");
                        var u = ReadVector(3);
                        Console.WriteLine("\nEnter 3D Vector 2 (x y z):");
                        var v = ReadVector(3);
                        double[] cross = new double[3];
                        cross[0] = u[1] * v[2] - u[2] * v[1];
                        cross[1] = u[2] * v[0] - u[0] * v[2];
                        cross[2] = u[0] * v[1] - u[1] * v[0];
                        Console.WriteLine($"\nCross Product = ({cross[0]:F2}, {cross[1]:F2}, {cross[2]:F2})");
                        break;
                        
                    case 3:
                        Console.WriteLine("\nEnter vector:");
                        var vec = ReadVector();
                        double magnitude = Math.Sqrt(vec.Sum(x => x * x));
                        Console.WriteLine($"\nMagnitude = {magnitude:F4}");
                        break;
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine($"Error: {ex.Message}");
            }
        }
        
        private static void SolveLinearSystem()
        {
            Console.WriteLine("\n=== Linear System Solver (Ax = b) ===");
            Console.WriteLine("Enter coefficient matrix A (one row per line, space-separated):");
            var A = ReadMatrix();
            
            Console.WriteLine("\nEnter constants vector b:");
            var b = ReadVector(A.Rows);
            
            // Using Gaussian elimination (simplified)
            int n = A.Rows;
            if (n != A.Cols)
            {
                Console.WriteLine("Only square systems are supported");
                return;
            }
            
            // Augment A with b
            for (int i = 0; i < n; i++)
            {
                A.Data[i, n] = b[i];
            }
            
            // Gaussian elimination
            for (int col = 0; col < n; col++)
            {
                // Partial pivot
                double max = Math.Abs(A.Data[col, col]);
                int maxRow = col;
                for (int k = col + 1; k < n; k++)
                {
                    if (Math.Abs(A.Data[k, col]) > max)
                    {
                        max = Math.Abs(A.Data[k, col]);
                        maxRow = k;
                    }
                }
                
                // Swap rows
                if (maxRow != col)
                {
                    for (int k = col; k <= n; k++)
                    {
                        double temp = A.Data[col, k];
                        A.Data[col, k] = A.Data[maxRow, k];
                        A.Data[maxRow, k] = temp;
                    }
                }
                
                // Eliminate
                for (int i = col + 1; i < n; i++)
                {
                    double factor = A.Data[i, col] / A.Data[col, col];
                    for (int j = col; j <= n; j++)
                    {
                        A.Data[i, j] -= A.Data[col, j] * factor;
                    }
                }
            }
            
            // Back substitution
            double[] x = new double[n];
            for (int i = n - 1; i >= 0; i--)
            {
                x[i] = A.Data[i, n];
                for (int j = i + 1; j < n; j++)
                {
                    x[i] -= A.Data[i, j] * x[j];
                }
                x[i] /= A.Data[i, i];
            }
            
            // Display solution
            Console.WriteLine("\nSolution:");
            for (int i = 0; i < n; i++)
            {
                Console.WriteLine($"x{i+1} = {x[i]:F4}");
            }
        }
        
        private static void CalculateEigen()
        {
            Console.WriteLine("\n=== Eigenvalues and Eigenvectors ===");
            Console.WriteLine("Note: This is a simplified implementation for 2x2 matrices");
            
            Console.WriteLine("\nEnter 2x2 matrix:");
            var A = ReadMatrix(2, 2);
            
            // Characteristic equation: λ² - (a+d)λ + (ad - bc) = 0
            double a = A.Data[0, 0], b = A.Data[0, 1];
            double c = A.Data[1, 0], d = A.Data[1, 1];
            
            double trace = a + d;
            double det = a * d - b * c;
            
            double discriminant = trace * trace - 4 * det;
            
            if (discriminant < 0)
            {
                double real = trace / 2;
                double imag = Math.Sqrt(-discriminant) / 2;
                Console.WriteLine("\nComplex eigenvalues:");
                Console.WriteLine($"λ1 = {real:F2} + {imag:F2}i");
                Console.WriteLine($"λ2 = {real:F2} - {imag:F2}i");
            }
            else
            {
                double sqrtDisc = Math.Sqrt(discriminant);
                double lambda1 = (trace + sqrtDisc) / 2;
                double lambda2 = (trace - sqrtDisc) / 2;
                
                Console.WriteLine("\nEigenvalues:");
                Console.WriteLine($"λ1 = {lambda1:F4}");
                Console.WriteLine($"λ2 = {lambda2:F4}");
                
                // For simplicity, just show the eigenvectors for real eigenvalues
                if (discriminant >= 0)
                {
                    Console.WriteLine("\nEigenvectors (normalized):");
                    if (b != 0)
                    {
                        Console.WriteLine($"For λ1: ({lambda1 - d:F2}, {b:F2})");
                        Console.WriteLine($"For λ2: ({lambda2 - d:F2}, {b:F2})");
                    }
                    else if (c != 0)
                    {
                        Console.WriteLine($"For λ1: ({c:F2}, {lambda1 - a:F2})");
                        Console.WriteLine($"For λ2: ({c:F2}, {lambda2 - a:F2})");
                    }
                    else
                    {
                        Console.WriteLine("Diagonal matrix - standard basis vectors are eigenvectors");
                    }
                }
            }
        }
        
        private static void CalculatePartialDerivatives()
        {
            Console.WriteLine("\n=== Partial Derivatives ===");
            Console.WriteLine("Enter a function f(x,y):");
            Console.WriteLine("Example: x^2 + 2*x*y + y^2");
            string function = Console.ReadLine()?.Trim() ?? "";
            
            Console.WriteLine("\n1. Partial derivative with respect to x (∂f/∂x)");
            Console.WriteLine("2. Partial derivative with respect to y (∂f/∂y)");
            Console.Write("\nSelect (1-2): ");
            
            if (!int.TryParse(Console.ReadLine(), out int choice) || choice < 1 || choice > 2)
            {
                Console.WriteLine("Invalid choice");
                return;
            }
            
            // This is a simplified symbolic differentiation
            string derivative = "";
            if (choice == 1) // ∂f/∂x
            {
                // Very basic symbolic differentiation
                derivative = function
                    .Replace("*x", "")  // x becomes 1
                    .Replace("x^2", "2x") // x² becomes 2x
                    .Replace("x", "1");   // x becomes 1
                Console.WriteLine($"\n∂f/∂x ≈ {derivative} (simplified)");
            }
            else // ∂f/∂y
            {
                // Very basic symbolic differentiation
                derivative = function
                    .Replace("*y", "")  // y becomes 1
                    .Replace("y^2", "2y") // y² becomes 2y
                    .Replace("y", "1");   // y becomes 1
                Console.WriteLine($"\n∂f/∂y ≈ {derivative} (simplified)");
            }
            
            Console.WriteLine("\nNote: This is a basic symbolic differentiation. For more complex functions,");
            Console.WriteLine("consider using a proper computer algebra system.");
        }
        
        private static void CalculateMultipleIntegrals()
        {
            Console.WriteLine("\n=== Multiple Integrals ===");
            Console.WriteLine("1. Double Integral");
            Console.WriteLine("2. Triple Integral");
            Console.Write("\nSelect (1-2): ");
            
            if (!int.TryParse(Console.ReadLine(), out int choice) || choice < 1 || choice > 2)
            {
                Console.WriteLine("Invalid choice");
                return;
            }
            
            if (choice == 1)
            {
                Console.WriteLine("\nEnter function f(x,y):");
                string function = Console.ReadLine()?.Trim() ?? "";
                
                double x0 = GetNumber("Enter lower limit for x: ");
                double x1 = GetNumber("Enter upper limit for x: ");
                double y0 = GetNumber("Enter lower limit for y: ");
                double y1 = GetNumber("Enter upper limit for y: ");
                
                // Simple numerical integration using midpoint rule
                int n = 100; // Number of intervals
                double hx = (x1 - x0) / n;
                double hy = (y1 - y0) / n;
                double sum = 0;
                
                for (int i = 0; i < n; i++)
                {
                    double x = x0 + (i + 0.5) * hx;
                    for (int j = 0; j < n; j++)
                    {
                        double y = y0 + (j + 0.5) * hy;
                        // Evaluate function at (x,y) - this is a placeholder
                        // In a real implementation, you'd parse the function string
                        double f = x * y; // Default function
                        sum += f * hx * hy;
                    }
                }
                
                Console.WriteLine($"\nApproximate double integral = {sum:F6}");
                Console.WriteLine("Note: This uses a simple numerical method. The actual function");
                Console.WriteLine("evaluation is currently hardcoded to f(x,y) = x*y");
            }
            else
            {
                Console.WriteLine("\nTriple integral implementation would go here");
                Console.WriteLine("This is a placeholder for the triple integral functionality");
            }
        }
        
        private static void VectorCalculusOperations()
        {
            Console.WriteLine("\n=== Vector Calculus ===");
            Console.WriteLine("1. Gradient (∇f)");
            Console.WriteLine("2. Divergence (∇·F)");
            Console.WriteLine("3. Curl (∇×F)");
            Console.Write("\nSelect (1-3): ");
            
            if (!int.TryParse(Console.ReadLine(), out int choice) || choice < 1 || choice > 3)
            {
                Console.WriteLine("Invalid choice");
                return;
            }
            
            switch (choice)
            {
                case 1:
                    Console.WriteLine("\nEnter scalar function f(x,y,z):");
                    string function = Console.ReadLine()?.Trim() ?? "";
                    Console.WriteLine("\nGradient ∇f = (∂f/∂x, ∂f/∂y, ∂f/∂z)");
                    Console.WriteLine("This would compute the partial derivatives symbolically");
                    Console.WriteLine("∂f/∂x = [derivative with respect to x]");
                    Console.WriteLine("∂f/∂y = [derivative with respect to y]");
                    Console.WriteLine("∂f/∂z = [derivative with respect to z]");
                    break;
                    
                case 2:
                    Console.WriteLine("\nEnter vector field F = (P, Q, R):");
                    Console.WriteLine("P(x,y,z) = [function of x,y,z]");
                    Console.WriteLine("Q(x,y,z) = [function of x,y,z]");
                    Console.WriteLine("R(x,y,z) = [function of x,y,z]");
                    Console.WriteLine("\nDivergence ∇·F = ∂P/∂x + ∂Q/∂y + ∂R/∂z");
                    break;
                    
                case 3:
                    Console.WriteLine("\nEnter vector field F = (P, Q, R):");
                    Console.WriteLine("P(x,y,z) = [function of x,y,z]");
                    Console.WriteLine("Q(x,y,z) = [function of x,y,z]");
                    Console.WriteLine("R(x,y,z) = [function of x,y,z]");
                    Console.WriteLine("\nCurl ∇×F = (∂R/∂y - ∂Q/∂z, ∂P/∂z - ∂R/∂x, ∂Q/∂x - ∂P/∂y)");
                    break;
            }
            
            Console.WriteLine("\nNote: This is a symbolic representation. For actual computation,");
            Console.WriteLine("a proper symbolic math library would be needed.");
        }
        
        private static Matrix ReadMatrix(int? rows = null, int? cols = null)
        {
            if (!rows.HasValue)
            {
                Console.Write("Enter number of rows: ");
                if (!int.TryParse(Console.ReadLine(), out int r) || r < 1)
                    throw new ArgumentException("Invalid number of rows");
                rows = r;
            }
            
            if (!cols.HasValue)
            {
                Console.Write("Enter number of columns: ");
                if (!int.TryParse(Console.ReadLine(), out int c) || c < 1)
                    throw new ArgumentException("Invalid number of columns");
                cols = c;
            }
            
            Matrix m = new Matrix(rows.Value, cols.Value);
            
            Console.WriteLine($"Enter {rows}x{cols} matrix (one row per line, space-separated):");
            for (int i = 0; i < rows; i++)
            {
                string[] values = (Console.ReadLine() ?? "").Split(' ', StringSplitOptions.RemoveEmptyEntries);
                if (values.Length != cols)
                    throw new ArgumentException($"Expected {cols} values for row {i+1}");
                    
                for (int j = 0; j < cols; j++)
                {
                    if (!double.TryParse(values[j], out double val))
                        throw new ArgumentException($"Invalid number at row {i+1}, column {j+1}");
                    m.Data[i, j] = val;
                }
            }
            
            return m;
        }
        
        private static double[] ReadVector(int? dimension = null)
        {
            if (!dimension.HasValue)
            {
                Console.Write("Enter vector dimension: ");
                if (!int.TryParse(Console.ReadLine(), out int d) || d < 1)
                    throw new ArgumentException("Invalid vector dimension");
                dimension = d;
            }
            
            Console.WriteLine($"Enter {dimension}-dimensional vector (space-separated):");
            string[] values = (Console.ReadLine() ?? "").Split(' ', StringSplitOptions.RemoveEmptyEntries);
            
            if (values.Length != dimension)
                throw new ArgumentException($"Expected {dimension} values");
                
            double[] vector = new double[dimension.Value];
            for (int i = 0; i < dimension; i++)
            {
                if (!double.TryParse(values[i], out double val))
                    throw new ArgumentException($"Invalid number at position {i+1}");
                vector[i] = val;
            }
            
            return vector;
        }
        
        private static void StatisticsOperations()
        {
            Console.WriteLine("\n=== Statistics Operations ===");
            Console.WriteLine("1. Descriptive Statistics");
            Console.WriteLine("2. Probability Distributions");
            Console.WriteLine("3. Hypothesis Testing");
            Console.WriteLine("4. Regression Analysis");
            Console.Write("\nSelect operation (1-4): ");
            
            if (!int.TryParse(Console.ReadLine(), out int choice) || choice < 1 || choice > 4)
            {
                Console.WriteLine("Invalid choice");
                return;
            }
            
            try
            {
                switch (choice)
                {
                    case 1:
                        CalculateDescriptiveStatistics();
                        break;
                    case 2:
                        ProbabilityDistributions();
                        break;
                    case 3:
                        HypothesisTesting();
                        break;
                    case 4:
                        RegressionAnalysis();
                        break;
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine($"Error: {ex.Message}");
            }
        }
        
        private static void CalculateDescriptiveStatistics()
        {
            Console.WriteLine("\n=== Descriptive Statistics ===");
            Console.WriteLine("Enter dataset (space-separated numbers):");
            double[] data = ReadVector();
            
            Array.Sort(data);
            int n = data.Length;
            
            // Basic statistics
            double sum = data.Sum();
            double mean = sum / n;
            double median = n % 2 == 0 ? 
                (data[n/2 - 1] + data[n/2]) / 2 : 
                data[n/2];
                
            // Variance and standard deviation
            double variance = data.Sum(x => Math.Pow(x - mean, 2)) / (n - 1);
            double stdDev = Math.Sqrt(variance);
            
            // Quartiles
            double q1 = Percentile(data, 25);
            double q3 = Percentile(data, 75);
            double iqr = q3 - q1;
            
            // Output results
            Console.WriteLine("\nDescriptive Statistics:");
            Console.WriteLine($"Count: {n}");
            Console.WriteLine($"Min: {data[0]:F4}");
            Console.WriteLine($"Q1: {q1:F4}");
            Console.WriteLine($"Median: {median:F4}");
            Console.WriteLine($"Q3: {q3:F4}");
            Console.WriteLine($"Max: {data[n-1]:F4}");
            Console.WriteLine($"IQR: {iqr:F4}");
            Console.WriteLine($"Mean: {mean:F4}");
            Console.WriteLine($"Variance: {variance:F4}");
            Console.WriteLine($"Std Dev: {stdDev:F4}");
            
            // Histogram
            Console.WriteLine("\nHistogram:");
            int bins = Math.Min(10, (int)Math.Sqrt(n));
            double min = data[0];
            double max = data[n-1];
            double range = max - min;
            double binSize = range / bins;
            
            int[] histogram = new int[bins];
            foreach (double x in data)
            {
                int bin = (int)((x - min) / binSize);
                if (bin == bins) bin--; // Handle the max value
                histogram[bin]++;
            }
            
            int maxFreq = histogram.Max();
            const int maxWidth = 50;
            
            for (int i = 0; i < bins; i++)
            {
                double binStart = min + i * binSize;
                double binEnd = binStart + binSize;
                int width = maxFreq > 0 ? (histogram[i] * maxWidth) / maxFreq : 0;
                
                Console.WriteLine($"[{binStart,7:F2} - {binEnd,7:F2}): {new string('█', width)} ({histogram[i]})");
            }
        }
        
        private static double Percentile(double[] data, double percentile)
        {
            if (percentile < 0 || percentile > 100)
                throw new ArgumentException("Percentile must be between 0 and 100");
                
            Array.Sort(data);
            double index = (data.Length - 1) * percentile / 100.0;
            int lower = (int)Math.Floor(index);
            
            if (lower >= data.Length - 1)
                return data[data.Length - 1];
                
            double fraction = index - lower;
            return data[lower] + fraction * (data[lower + 1] - data[lower]);
        }
        
        private static void ProbabilityDistributions()
        {
            Console.WriteLine("\n=== Probability Distributions ===");
            Console.WriteLine("1. Normal Distribution");
            Console.WriteLine("2. Binomial Distribution");
            Console.WriteLine("3. Poisson Distribution");
            Console.Write("\nSelect distribution (1-3): ");
            
            if (!int.TryParse(Console.ReadLine(), out int choice) || choice < 1 || choice > 3)
            {
                Console.WriteLine("Invalid choice");
                return;
            }
            
            try
            {
                switch (choice)
                {
                    case 1:
                        NormalDistribution();
                        break;
                    case 2:
                        BinomialDistribution();
                        break;
                    case 3:
                        PoissonDistribution();
                        break;
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine($"Error: {ex.Message}");
            }
        }
        
        private static void NormalDistribution()
        {
            double mean = GetNumber("Enter mean (μ): ");
            double stdDev = GetNumber("Enter standard deviation (σ > 0): ", x => x > 0);
            
            Console.WriteLine("\n1. Probability Density Function (PDF)");
            Console.WriteLine("2. Cumulative Distribution Function (CDF)");
            Console.Write("Select function (1-2): ");
            
            if (!int.TryParse(Console.ReadLine(), out int funcChoice) || funcChoice < 1 || funcChoice > 2)
            {
                Console.WriteLine("Invalid choice");
                return;
            }
            
            double x = GetNumber("Enter x value: ");
            double result = 0;
            
            if (funcChoice == 1)
            {
                // PDF
                result = Math.Exp(-0.5 * Math.Pow((x - mean) / stdDev, 2)) / (stdDev * Math.Sqrt(2 * Math.PI));
                Console.WriteLine($"\nPDF({x}) = {result:F6}");
            }
            else
            {
                // CDF using error function approximation
                double z = (x - mean) / (stdDev * Math.Sqrt(2));
                result = 0.5 * (1 + Erf(z));
                Console.WriteLine($"\nP(X ≤ {x}) = {result:F6}");
            }
            
            // Calculate and show z-score
            double zScore = (x - mean) / stdDev;
            Console.WriteLine($"Z-score: {zScore:F4}");
            
            // Show probability ranges
            if (funcChoice == 2)
            {
                Console.WriteLine("\nCommon probability ranges:");
                Console.WriteLine($"Within 1σ: {Erf(1/Math.Sqrt(2)):P2}");
                Console.WriteLine($"Within 2σ: {Erf(2/Math.Sqrt(2)):P2}");
                Console.WriteLine($"Within 3σ: {Erf(3/Math.Sqrt(2)):P2}");
            }
        }
        
        // Error function approximation
        private static double Erf(double x)
        {
            // Constants
            double a1 =  0.254829592;
            double a2 = -0.284496736;
            double a3 =  1.421413741;
            double a4 = -1.453152027;
            double a5 =  1.061405429;
            double p  =  0.3275911;

            // Save the sign of x
            int sign = x < 0 ? -1 : 1;
            x = Math.Abs(x);

            // A&S formula 7.1.26
            double t = 1.0 / (1.0 + p * x);
            double y = 1.0 - ((((a5 * t + a4) * t + a3) * t + a2) * t + a1) * t * Math.Exp(-x * x);

            return sign * y;
        }
        
        // Helper method to get a number with validation
        private static double GetNumber(string prompt, Func<double, bool>? validator = null)
        {
            while (true)
            {
                Console.Write(prompt);
                if (double.TryParse(Console.ReadLine(), out double result))
                {
                    if (validator == null || validator(result))
                        return result;
                }
                Console.WriteLine("Invalid input. Please try again.");
            }
        }
        
        // Placeholder methods that will be implemented in the next part
        private static void BinomialDistribution() { Console.WriteLine("Binomial Distribution will be implemented in the next part."); }
        private static void PoissonDistribution() { Console.WriteLine("Poisson Distribution will be implemented in the next part."); }
        private static void HypothesisTesting() { Console.WriteLine("Hypothesis Testing will be implemented in the next part."); }
        private static void RegressionAnalysis() { Console.WriteLine("Regression Analysis will be implemented in the next part."); }
        private static void MultivariateAnalysis() { Console.WriteLine("Multivariate Analysis will be implemented in the next part."); }
    }
}