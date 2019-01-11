#load "lib.fs"
open Lib

let isDivisibleByAny divisors number =
    divisors
    |> Seq.map isDivisibleBy
    |> Seq.exists (fun f -> f number)

let sumMatches f = Seq.filter f >> Seq.reduce (+)

[1..999]
|> sumMatches (isDivisibleByAny [3; 5])
|> printfn "%O"
