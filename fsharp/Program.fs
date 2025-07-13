// For more information see https://aka.ms/fsharp-console-apps
let eatFish() =
    printfn "Eating fish"
    printfn "Eating more fish"
    printfn "Eating even more fish"
    printfn "Eating even more fish"
let hello() =
    printfn "Hello from F#"
    System.Console.WriteLine("Hello from F#")
    printf "Hello from F#"
    let firstName = "John"
    let lastName = "Doe"
    printfn "Hello %s %s, nice to meet you" firstName lastName
    printf "Great regards %s %s" firstName lastName
    let age = 17
    let height = 1.205
    let mutable weight = 40.2
    let grade = 'A'
    let isMajor = age >= 18
    let isMarried = true
    let children = 4
    let exGfs = 14
    let isMinor = age < 18
    let childrenNames = ("John", "Jane", "Joe", "Junior")
    let johnsPi = 3.141592653589793238462643383279502884197169399375105820974944M
    printfn "Children names: %A" childrenNames
    printfn "Is minor: %b" isMinor
    printfn "Is major: %b" isMajor
    printfn "Is married: %2b" isMarried
    printfn "Children: %*d" children children
    printfn "Height: %.2f" height
    printfn "Grade: %-5c" grade
    printfn "Ex Girlfriends: %i" exGfs
    printfn "Johns Pi: %M" johnsPi
    eatFish()
    weight <- weight + 90.0
[<EntryPoint>]
let main argv =
    hello()
    let stack = ref 1
    let big = pown 2 16
    let a = 9.0 ** 0.5
    printfn "a: %f" a
    let mutable i = 0
    let array = [| 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 |]
    printfn "Array: %A" array
    0
    let rec loop() =
        stack := !stack <<< 2
        i <- i + 1
        let s = pown 2 2
        printfn "Stack: %i" !stack
        printfn "i: %i" i
        if !stack < big then
            loop()
    loop()
    printfn "Stack: %d" !stack
    System.Console.ReadKey()
    0
