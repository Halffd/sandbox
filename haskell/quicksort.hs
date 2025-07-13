import System.CPUTime
import Text.Printf
import System.Random (randomRIO)
import Control.DeepSeq (deepseq, NFData)

-- Quicksort
qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
    where
        smaller = filter (<= x) xs
        larger  = filter (> x) xs

-- Insertion sort
isort :: Ord a => [a] -> [a]
isort []     = []
isort (x:xs) = insert x (isort xs)

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) = if x <= y then x : y : ys else y : insert x ys

-- Riffle shuffle
randomise :: [a] -> [a]
randomise xs = shuffle 5 xs

shuffle :: Int -> [a] -> [a]
shuffle 0 xs = xs
shuffle n xs = shuffle (n - 1) (riffle xs)

riffle :: [a] -> [a]
riffle [] = []
riffle xs = interleave ys zs
    where (ys, zs) = splitAt (length xs `div` 2) xs

interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave xs [] = xs
interleave (x:xs) ys = x : interleave ys xs

-- Timing utility
timeIt :: NFData a => String -> ([Int] -> a) -> [Int] -> IO ()
timeIt label f xs = do
    start <- getCPUTime
    let result = f xs
    result `deepseq` return ()
    end <- getCPUTime
    let diff = fromIntegral (end - start) / (10^12)
    printf "%s took %.3f sec\n" label (diff :: Double)

main :: IO ()
main = do
    let sizes = [10, 5000, 250000, 10000000]
    mapM_ testSorts sizes

testSorts :: Int -> IO ()
testSorts n = do
    putStrLn $ "\nList size: " ++ show n
    let xs = [1..n]
        shuffled = randomise xs
    shuffled `deepseq` return ()  -- force evaluation

    timeIt "qsort" qsort shuffled
    if n <= 5000
        then timeIt "isort" isort shuffled
        else putStrLn "isort skipped (too slow)"
