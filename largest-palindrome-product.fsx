#load "assertions.fs"
#load "lib.fs"
open Assertions
open Lib

largestPalindromeProduct [1..11] |> Is 99
largestPalindromeProduct [10..99] |> Is 9009
largestPalindromeProduct [100..999] |> Is 906609
