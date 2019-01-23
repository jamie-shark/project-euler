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

triangularNumbers |> Seq.take 10 |> ContainsTheSameItemsAs [1;3;6;10;15;21;28;36;45;55]
