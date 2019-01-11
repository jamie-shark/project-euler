let rec fibs =
    Seq.cache <| seq {
        yield 0
        yield 1
        yield! Seq.map2 (+) fibs (fibs |> Seq.skip 1)
    }

let isEven x = x % 2 = 0

fibs
|> Seq.takeWhile ((>) 4000000)
|> Seq.filter isEven
|> Seq.reduce (+)
|> printfn "%A"
