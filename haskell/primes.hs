
module Main where
import Control.Monad.ST
import Data.Array.ST
import Control.Monad 
import Data.Time.Clock 
import Data.STRef

pythonPrimes :: Int -> [Int]
pythonPrimes n = runST $ do
    arr <- newArray (2,n) False :: ST s (STArray s Int Bool)
    acc <- newSTRef []
    forM_ [2..n] $ \i -> do
        continue <- readArray arr i
        unless continue $
            do
                modifySTRef acc (i:)
                let i' = i + i
                forM_ [i',i'+i..n] $ \j -> 
                    writeArray arr j True

    readSTRef acc

main :: IO ()
main = do
    timeStart <- getCurrentTime
    print (length (pythonPrimes 999999))
    timeEnd <- getCurrentTime
    print (diffUTCTime timeEnd timeStart)
