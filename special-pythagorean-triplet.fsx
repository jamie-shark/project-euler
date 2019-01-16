#load "assertions.fs"
#load "lib.fs"
open Assertions
open Lib

let isTotal x = Seq.reduce (+) >> (=) x

let toListOfLists = Seq.map Seq.toList >> Seq.toList

let square x = x * x

let isWholeNumber x = floor x = x

let maybeGetPythagoreanTriple = function
    | a::b::[] ->
        let c = square a + square b |> float |> sqrt
        if isWholeNumber c
            then a::b::(int c)::[] |> Some
            else None
    | _        -> None

let getPythagoreanTriplesTotaling total =
    combinations 2 [1..total]
    |> Seq.map Seq.sort
    |> Seq.choose (Seq.toList >> maybeGetPythagoreanTriple)
    |> Seq.filter (isTotal total)

let stopWatch = System.Diagnostics.Stopwatch.StartNew()

isWholeNumber 1.0 |> Is true
isWholeNumber 1.1 |> Is false

getPythagoreanTriplesTotaling 12 |> toListOfLists |> Is [[3;4;5]]
getPythagoreanTriplesTotaling 144|> toListOfLists |> Is [[16; 63; 65]; [36; 48; 60]]
getPythagoreanTriplesTotaling 240|> toListOfLists |> Is [[15; 112; 113];
                                                         [40; 96; 104] ;
                                                         [48; 90; 102] ;
                                                         [60; 80; 100] ]

getPythagoreanTriplesTotaling 1000 |> toListOfLists |> Is [[200; 375; 425]]

stopWatch.Stop()
printfn "Tests run in %fms" stopWatch.Elapsed.TotalMilliseconds

