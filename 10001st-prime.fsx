#load "assertions.fs"
#load "lib.fs"
open Assertions
open Lib

let nPrime x =
    allNumbers
    |> Seq.filter isPrime
    |> Seq.item (x - 1)

isPrime 2 |> Is true
isPrime 3 |> Is true
isPrime 4 |> Is false
isPrime 13 |> Is true
isPrime 14 |> Is false

nPrime 6 |>  Is 13
nPrime 10001 |>  Is 104743
