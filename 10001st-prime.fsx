#load "assertions.fs"
#load "lib.fs"
open Assertions
open Lib

let nPrime x =
    allNumbers
    |> Seq.filter isIntPrime
    |> Seq.item (x - 1)

isIntPrime 2 |> Is true
isIntPrime 3 |> Is true
isIntPrime 4 |> Is false
isIntPrime 13 |> Is true
isIntPrime 14 |> Is false

nPrime 6 |>  Is 13
nPrime 10001 |>  Is 104743
