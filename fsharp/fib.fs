module Fib

let rec fibonacci n =
    printfn "Computing fibonacci(%d)" n
    if n < 2 then n else fibonacci(n - 1) + fibonacci(n - 2)

printfn "Result: %d" (fibonacci 5)
