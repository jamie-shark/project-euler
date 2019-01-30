#load "assertions.fs"
#load "lib.fs"
open Assertions
open Lib

let nextCollatz = function
    | Even n -> (n / 2) |> int
    | Odd n  -> (3 * n) + 1

let rec collatzSequence n =
    Seq.cache <| seq {
        match n with
        | 1 -> yield 1
        | n -> yield n
               yield! (collatzSequence (nextCollatz n))
    }

let longestCollatzBelow n =
    [1..n-1]
    |> Seq.map collatzSequence
    |> Seq.maxBy Seq.length
    |> Seq.head

collatzSequence 13 |> Seq.toList |> Is [13;40;20;10;5;16;8;4;2;1]

longestCollatzBelow 14 |> Is 9

//longestCollatzBelow 1000000 |> Is 1
