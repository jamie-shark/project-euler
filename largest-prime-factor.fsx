#load "assertions.fs"
#load "lib.fs"
open Assertions
open Lib

largestPrimeFactor 12UL |> Is 3UL
largestPrimeFactor 13195UL |> Is 29UL
largestPrimeFactor 600851475143UL |> Is 6857UL
