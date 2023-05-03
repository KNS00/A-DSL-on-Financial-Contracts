module testFunctions

let listsAreEqual (xs: float list) (ys: float list) (tolerance: float) : bool = // checks if two lists are approximately equal
    match List.length xs, List.length ys with
    | xlen, ylen when xlen <> ylen -> failwith "Lists must have the same length"
    | _ -> xs |> List.forall2 (fun x y -> abs(x - y) / y <= tolerance) ys

let simulation (f: 'a -> float) (input : 'a) : float = // runs a function 1_000_000 times and returns the average output
    let n = 1_000_000
    [for _ in 1..n ->
        let res = f input
        res]
    |> List.average