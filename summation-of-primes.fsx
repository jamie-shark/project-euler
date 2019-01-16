#load "assertions.fs"
#load "lib.fs"
open Assertions
open Lib

let sumPrimes max =
    allUInts
    |> Seq.filter isPrime
    |> Seq.takeWhile ((>) max)
    |> Seq.reduce (+)

sumPrimes 10UL |> Is 17UL
//sumPrimes 2000000UL |> Is 142913828922UL
