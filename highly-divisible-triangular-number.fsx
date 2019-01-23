#load "assertions.fs"
#load "lib.fs"
open Assertions
open Lib

let triangularNumbers =
    let rec next current n =
        seq {
            let value = current + n
            yield value
            yield! next value (n+1)
        }
    next 0 1

let factorsOf n =
    let rec helper factors = function
        | x when x > (n / 2)       -> factors
        | x when isDivisibleBy x n -> helper (x::factors) (x+1)
        | x                        -> helper  factors     (x+1)
    helper (n::[]) 1

triangularNumbers |> Seq.take 10 |> ContainsTheSameItemsAs [1;3;6;10;15;21;28;36;45;55]

factorsOf 1  |> ContainsTheSameItemsAs [1]
factorsOf 3  |> ContainsTheSameItemsAs [1;3]
factorsOf 6  |> ContainsTheSameItemsAs [1;2;3;6]
factorsOf 10 |> ContainsTheSameItemsAs [1;2;5;10]
factorsOf 15 |> ContainsTheSameItemsAs [1;3;5;15]
factorsOf 21 |> ContainsTheSameItemsAs [1;3;7;21]
factorsOf 28 |> ContainsTheSameItemsAs [1;2;4;7;14;28]
