module Lib

let flip f a b = f b a
let (><) = flip

let allNumbers =
    let rec next x =
        seq {
            let value = x + 1
            yield value
            yield! next value
        }
    next 0

let allUInts =
    let rec next x =
        seq {
            let value = x + 1UL
            yield value
            yield! next value
        }
    next 0UL

let isDivisibleBy divisor number = (number % divisor) = 0

let intPrimeFactors =
    let rec primeFactors' factors counter = function
        | n when n < counter     -> factors
        | n when n % counter = 0 -> primeFactors' (counter::factors) counter     (n/counter)
        | n                      -> primeFactors'  factors          (counter+1)   n
    primeFactors' [] 2

let primeFactors =
    let rec primeFactors' factors counter = function
        | n when n < counter       -> factors
        | n when n % counter = 0UL -> primeFactors' (counter::factors) counter       (n/counter)
        | n                        -> primeFactors'  factors          (counter+1UL)   n
    primeFactors' [] 2UL

let largestPrimeFactor = primeFactors >> Seq.max

let inline isOneItem xs = Seq.length xs |> (=) 1

let isPrime = primeFactors >> isOneItem
let isIntPrime = intPrimeFactors >> isOneItem

let isPalindrome number =
    let asString = string number
    asString.ToCharArray()
    |> Array.rev
    |> Array.map (string)
    |> String.concat ""
    |> (=) asString

let combinations =
    let rec combinations' results size set =
        seq {
            match size, set with
            | n, x::xs -> if n > 0  then yield! combinations' (x::results) (n - 1) xs
                          if n >= 0 then yield! combinations'  results      n      xs
            | 0, []    -> yield results
            | _, []    -> ()
        }
    combinations' []

let largestPalindromeProduct =
    combinations 2
    >> Seq.map (Seq.reduce ( *))
    >> Seq.filter isPalindrome
    >> Seq.max

let charsToString (xs:char seq) =
    xs
    |> Seq.map string
    |> Seq.reduce (+)

