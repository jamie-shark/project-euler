module Assertions

let Is expected actual =
    match expected = actual with
    | true -> printf "."
    | false -> printfn "F\nExpected %A but got %A" expected actual

let ContainsTheSameItemsAs (expected:seq<'a>) (actual:seq<'a>) =
    let areAllActualItemsInExpected =
        actual |> Seq.forall (fun x -> Seq.contains x expected)
    let areBothSameLength =
        (Seq.length expected) = (Seq.length actual)

    if areAllActualItemsInExpected && areBothSameLength
        then printf "."
        else printfn "F\nExpected %A but got %A" expected actual

