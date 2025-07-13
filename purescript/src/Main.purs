module Main where

import Prelude
import Effect (Effect)
import Effect.Console (logShow)

fibonacci :: Int -> Int
fibonacci n
  | n < 2     = n
  | otherwise = fibonacci (n - 1) + fibonacci (n - 2)

main :: Effect Unit
main = logShow (fibonacci 5)
