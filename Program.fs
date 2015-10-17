open System
open System.Numerics

let rec fib = function 
    | n when n = 0I -> 0I
    | n when n = 1I -> 1I
    | n -> fib(n - 1I) + fib(n - 2I)

let mutable r = 1I

let fibs(j:bigint):seq<bigint> =
    seq { for k in [0I..j] do
            yield (r)
            r <- r + k
            }

let filter(n:seq<bigint>):seq<bigint> =
    seq { for i in n do
            if i % 2I = 0I then
                yield i}



let sum(n:seq<bigint>):bigint =
    Seq.sum(n)


[<EntryPoint>]
let main argv = 
    printfn "Find the sum of even-valued Fibonacci numbers \r"
    printfn" where the highest-valued term does not exceed 20,000"
    printfn "" 
    printfn "Here is the Fibonacci Sequence for numbers 1-10: "
    Seq.iter(fun n -> printfn "%A" n) (fibs 10I)
    printfn ""
    printfn "The sum of the even Fibonacci terms are: %A" (fibs r |> filter |> sum)
    0 // return an integer exit code

