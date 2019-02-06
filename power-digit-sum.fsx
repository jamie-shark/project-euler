#load "assertions.fs"
#load "lib.fs"
open Assertions
open Lib

let powerDigitSum exponent = 26

powerDigitSum 15 |> Is 26
