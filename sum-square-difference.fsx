#load "assertions.fs"
#load "lib.fs"
open Assertions
open Lib

let sum = Seq.reduce (+)
let square x = x * x

let sumSquares =
    Seq.map square >> sum

let squareSum =
    sum >> square

sumSquares [1..10] |> Is 385
squareSum [1..10] |> Is 3025

squareSum [1..10] - sumSquares [1..10] |> Is 2640
squareSum [1..100] - sumSquares [1..100] |> Is 25164150
