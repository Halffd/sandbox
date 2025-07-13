module Main exposing (main)

import Html exposing (text)

fibonacci : Int -> Int
fibonacci n =
    if n < 2 then n else fibonacci (n - 1) + fibonacci (n - 2)

main =
    text ("Fibonacci(5) = " ++ String.fromInt (fibonacci 5))
