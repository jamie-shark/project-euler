#load "assertions.fs"
#load "lib.fs"
open Assertions
open Lib

let toIntDigits = char >> string >> int

let powerDigitSum exponent =
    2I ** exponent
    |> string
    |> Seq.map toIntDigits
    |> Seq.sum


2I ** 2 |> Is 4I
2I ** 4 |> Is 16I
powerDigitSum 2 |> Is 4
powerDigitSum 4 |> Is 7
powerDigitSum 15 |> Is 26
powerDigitSum 1000 |> Is 1366
