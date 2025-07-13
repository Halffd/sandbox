fibonacci :: Int -> IO Int
fibonacci n = do
  putStrLn $ "Computing fibonacci(" ++ show n ++ ")"
  if n < 2
    then return n
    else do
      a <- fibonacci (n - 1)
      b <- fibonacci (n - 2)
      return (a + b)
main :: IO ()
main = do
  result <- fibonacci 4
  putStrLn $ "Result: " ++ show result
