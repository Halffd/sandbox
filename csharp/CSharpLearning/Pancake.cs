using System;

class Pancake
{
    public static int tally = 0;
public static int caseNum = 1;

public static void Run() {
    TextWriter tmp = Console.Out;
    StreamWriter sw = new StreamWriter(Console.OpenStandardOutput());
    Console.SetOut(sw);
    using (StreamReader stdinput = new StreamReader(Console.OpenStandardInput())) 
    {
        string line;
        while ((line = stdinput.ReadLine()) != null) 
        {
            // Check for exit conditions
            if (line.Trim().Equals("quit", StringComparison.OrdinalIgnoreCase) || 
                line.Trim().Equals("q", StringComparison.OrdinalIgnoreCase))
            {
                break;
            }

            try
            {
                var liner = line.Split(new[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);
                
                // Skip empty lines or invalid input
                if (liner.Length < 2) continue;
                
                // Check if the last element is a valid number
                if (!int.TryParse(liner[^1], out int K) || K <= 0)
                {
                    Console.WriteLine($"Invalid K value in line: {line}");
                    continue;
                }
                
                var pancakes = liner[0];
                
                // Validate that the pancake string only contains '+' or '-'
                if (pancakes.Any(c => c != '+' && c != '-'))
                {
                    Console.WriteLine($"Skipping invalid input: {pancakes} - must contain only '+' or '-' characters");
                    continue;
                }
                
                // Process the pancakes
                string result = pancakes;
                while (result != "IMPOSSIBLE" && result != "END") 
                {
                    result = lineup(result, K);
                }
                
                Console.WriteLine("Case #{0}: {1}", caseNum, result == "IMPOSSIBLE" ? "IMPOSSIBLE" : tally.ToString());
                tally = 0;
                caseNum += 1;
            }
            catch (Exception ex) when (ex is FormatException || ex is OverflowException)
            {
                Console.WriteLine($"Error processing line: {line}");
                Console.WriteLine($"Error details: {ex.Message}");
                continue;
            }
        }
    }
    sw.Close();
}

static string lineup(string pancakes, int K) {
 if (pancakes.Length <= 0 || pancakes.Where(k => k == '-').Count() == 0) {
 return "END";
 }

 if (pancakes.Length < K) {
 return "IMPOSSIBLE";
 }

 if (pancakes.ElementAt(0) == '+') {
 return pancakes.Substring(1);
 }

 tally += 1;
 return flipup(pancakes.Substring(0, K)) + pancakes.Substring(K);
}

static string flipup(string pancakes) {
 return string.Concat(pancakes.Select(k => k == '+' ? '-' : '+').ToList());
}
}