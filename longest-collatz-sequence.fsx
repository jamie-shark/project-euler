#load "assertions.fs"
#load "lib.fs"
open Assertions
open Lib

let nextCollatz = function
    | EvenLong n -> (n / 2UL) |> uint64
    | OddLong n  -> (3UL * n) + 1UL

let collatzSequence =
    let cache = newCache()
    let rec collatzSequence' = memoise cache (fun n ->
        seq {
            match n with
            | 1UL -> yield 1UL
            | n -> yield n
                   yield! nextCollatz n |> collatzSequence'
        })
    collatzSequence'

let longestCollatzBelow n =
    [1UL..n-1UL]
    |> Seq.map (fun n -> (n, collatzSequence n |> Seq.length))
    |> Seq.maxBy snd
    |> fst

collatzSequence 13UL |> Seq.toList |> Is [13UL;40UL;20UL;10UL;5UL;16UL;8UL;4UL;2UL;1UL]

longestCollatzBelow 14UL |> Is 9UL

//longestCollatzBelow 1000000UL |> Is 837799UL
