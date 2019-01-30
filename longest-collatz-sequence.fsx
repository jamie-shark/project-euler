#load "assertions.fs"
#load "lib.fs"
open Assertions
open Lib

let collatzSequence =
    let nextCollatz = function
        | Even n -> (n / 2) |> int
        | Odd n  -> (3 * n) + 1
    let rec sequence ns = function
        | 1 -> (1::ns)
        | n -> sequence (n::ns) (nextCollatz n)
    sequence []

collatzSequence 13 |> Is [13;40;20;10;5;16;8;4;2;1]

(*
[1..1000000]
    |> Seq.map collatzSequence
    |> Seq.
    |> Is 1
*)
