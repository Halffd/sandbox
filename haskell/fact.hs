import Data.Function
import Data.Functor

-- Recursive definition (elegant but inefficient)
f :: Int -> Int
f n
  | n < 3     = n
  | otherwise = [1..3] <&> (\i -> i * f (n - i)) & sum

-- Iterative definition (efficient)
f' :: Int -> Int
f' n
  | n < 3     = n
  | otherwise = iter 3 2 1 0
    where
      iter count f1 f2 f3
        | count == n = f0
        | otherwise  = iter (count+1) f0 f1 f2
          where f0 = f1 + 2 * f2 + 3 * f3

-- Verbose iterative definition with printing
fWithSteps :: Int -> IO Int
fWithSteps n
  | n < 3     = do
      putStrLn $ "Base case: f(" ++ show n ++ ") = " ++ show n
      return n
  | otherwise = do
      putStrLn "Starting iterative calculation:"
      putStrLn $ "f(0) = 0, f(1) = 1, f(2) = 2"
      iter 3 2 1 0
    where
      iter count f1 f2 f3 = do
        let f0 = f1 + 2 * f2 + 3 * f3
        putStrLn $ "f(" ++ show count ++ ") = " 
                 ++ show f1 ++ " + 2*" ++ show f2 ++ " + 3*" ++ show f3 
                 ++ " = " ++ show f0
        if count == n 
          then return f0
          else iter (count+1) f0 f1 f2

-- Function to print a sequence of values
printSequence :: Int -> IO ()
printSequence maxN = do
  putStrLn "Computing sequence using efficient iterative method:"
  putStrLn "n\tf(n)"
  putStrLn "-------------"
  mapM_ (\n -> putStrLn $ show n ++ "\t" ++ show (f' n)) [0..maxN]

-- Function to compare both implementations
compareImplementations :: Int -> IO ()
compareImplementations n = do
  putStrLn $ "Comparing calculations for f(" ++ show n ++ "):"
  putStrLn $ "Recursive: f(" ++ show n ++ ") = " ++ show (f n)
  putStrLn $ "Iterative: f'(" ++ show n ++ ") = " ++ show (f' n)
  
-- Main function with examples
main :: IO ()
main = do
  putStrLn "Exercise 1.11 - Computing f(n) = f(n-1) + 2*f(n-2) + 3*f(n-3)"
  putStrLn "-------------------------------------------------------------"
  
  -- Print a sequence
  printSequence 10
  
  putStrLn "\n"
  
  -- Show detailed steps for calculating f(6)
  putStrLn "Detailed calculation of f(6):"
  result <- fWithSteps 6
  putStrLn $ "Final result: f(6) = " ++ show result
  
  putStrLn "\n"
  
  -- Compare implementations for a few values
  mapM_ compareImplementations [5, 10, 15]