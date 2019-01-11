#load "assertions.fs"
#load "lib.fs"
open Assertions
open Lib

let isEvenlyDivisibleByAll divisors number =
    Seq.forall (isDivisibleBy >< number) divisors

let smallestNumberEvenlyDivisibleBy xs =
    allNumbers |> Seq.find (isEvenlyDivisibleByAll xs)

isEvenlyDivisibleByAll [1;2;3] 6 |> Is true
isEvenlyDivisibleByAll [1;2;3] 7 |> Is false
isEvenlyDivisibleByAll [1;10;20;5;4;2] 20 |> Is true
isEvenlyDivisibleByAll [1..10] 2520 |> Is true

smallestNumberEvenlyDivisibleBy [1..3] |> Is 6
smallestNumberEvenlyDivisibleBy [1..10] |> Is 2520
smallestNumberEvenlyDivisibleBy [1..20] |> Is 232792560
