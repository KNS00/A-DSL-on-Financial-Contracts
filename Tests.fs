module Tests
open DSL

let rec eval (o : Obs) = // function to evaluate observables
    match o with
    | Value n -> n
    // Missing Underlying
    | Mul (c1, c2) ->
        let n1 = eval c1
        let n2 = eval c2
        n1 * n2
    | Add (c1, c2) ->
        let n1 = eval c1
        let n2 = eval c2
        n1 + n2
    | Sub (c1, c2) ->
        let n1 = eval c1
        let n2 = eval c2
        n1 - n2
    | Max (c1, c2) ->
        let n1 = eval c1
        let n2 = eval c2
        max (n1) (n2)

// Test case 1: Simple arithmetic
let obs1 = Add(Value 2.0, Mul(Value 3.0, Value 4.0))
let result1 = eval obs1
// Expected: 14 (2 + (3 * 4) = 14)

// Test case 2: Nested expressions
let obs2 = Max(Value 5.0, Sub(Value 10.0, Mul(Value 2.0, Value 3.0)))
let result2 = eval obs2
// Expected: 1 (5 is greater than 10 - (2 * 3) = 4)

let printer =
    printfn "%A" result1
    printfn "%A" result2