#load "assertions.fs"
#load "lib.fs"
open Assertions
open Lib

let rec collatzSequence n =
    let next = function
        | EvenLong n -> (n / 2UL) |> uint64
        | OddLong n  -> (3UL * n) + 1UL
    seq {
        match n with
        | 1UL -> yield 1UL
        | n   -> yield n
                 yield! next n |> collatzSequence
    }

let longestCollatzSequenceBelow n =
    [1UL..n-1UL] |> Seq.maxBy (collatzSequence >> Seq.length)

collatzSequence 13UL |> Seq.toList |> Is [13UL;40UL;20UL;10UL;5UL;16UL;8UL;4UL;2UL;1UL]

longestCollatzSequenceBelow 14UL |> Is 9UL

let sw = System.Diagnostics.Stopwatch.StartNew()
longestCollatzSequenceBelow 1000000UL |> Is 837799UL
sw.Stop()
printfn "%fms" sw.Elapsed.TotalMilliseconds
