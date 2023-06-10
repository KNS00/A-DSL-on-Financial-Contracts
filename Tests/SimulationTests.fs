module simulationTests
open Simulations
open FsUnit
open Xunit

module simulationTests =
    // checks if two lists are approximately equal
    let listsAreEqual (xs: float list) (ys: float list) (tolerance: float) : bool = 
        match List.length xs, List.length ys with
        | xlen, ylen when xlen <> ylen ->
            printfn "%A" xlen
            printfn "%A" ylen
            failwith "Lists must have the same length"
        | _ -> xs |> List.forall2 (fun x y -> abs(x - y) / y <= tolerance) ys


    
    (* The following functions are used to test the distribution of the increments of the wiener process *)
    // subtract columns, helper function

    // Step two: pair wise difference
    let pairwiseDiff (input: float list) =
        match input with
        | [] | [_] -> [] 
        | x :: xs ->
            let rec loop prev acc = function
                | [] -> List.rev acc
                | y :: ys ->
                    let diff = prev - y
                    loop y (diff :: acc) ys
            loop x [] xs
    // finally subtract columns
    let subCols (input: float list list) =
        List.map pairwiseDiff input

    // Step 3: find the distribution
    let dist (input: float list list) =
        let numRows = List.length input        
        let stats (column: float list) =
            let sum = List.sum column
            let average = sum / float numRows
            let squaredDiffs = List.map (fun x -> (x - average) ** 2.0) column
            let variance = List.sum squaredDiffs / float numRows
            (average, variance)
        
        let transposed = List.transpose input
        List.map stats transposed
    
    // step 4: check distribution
    let checkDist (dt : float) (distribution : (float * float) list) : bool =
        let isDist (x, y) =
            abs(x - 0.0) < 1e-2 && abs(y - dt) < 1e-2
    
        distribution
        |> List.map isDist
        |> List.forall (fun result -> result)
        
    [<Theory>] 
    [<InlineData(1, 1.0)>]
    [<InlineData(2, 1.0)>]    
    [<InlineData(10, 0.5)>]    
    [<InlineData(15, 0.1)>]    
    [<InlineData(10, 2.0)>]    
    let ``Wiener Process: N(0, t-s) at any time.``(endTime, dt) =
        let n =
            match dt with
            | _ when dt > 1.0 -> 1_000_000
            | _ -> 100_000
        let simulation : float list list =
        
            List.init n (fun _ -> WienerProcess endTime dt)
        let sub = subCols(simulation)
        let distribution : (float * float) List = dist sub
        let test : bool = checkDist dt distribution
        test |> should equal true
        
        

    [<Theory>]
    [<InlineData(100.0, 1, 0.2, 0.1)>]     
    [<InlineData(100.0, 10, 0.03, 0.02)>]     
    [<InlineData(100.0, 365, 0.07, 0.05)>]    
    [<InlineData(100.0, 177, 0.2, 0.1)>]     
    let ``Assert GBM Expected Value``(S0, endTime, mu, sigma) =
        let exp = S0 * exp(float mu * float(endTime) / 365.)
        let n = 10_000
        let sim : float =
            List.init n (fun _ -> GBM S0 endTime mu sigma)
            |> List.average
        printfn "%A" exp
        printfn "%A" sim
        let test : bool = abs(exp - sim) / sim <= 10e-2
        test |> should equal true 
        
       
        
   [<Theory>] 
   [<InlineData(100.0, 1, 0.1, 1.0, 0.05)>]  
   [<InlineData(15.0, 365, 1.0, -0.2, 0.1)>] 
   [<InlineData(10.0, 100, 1.0, 0.3, 0.02)>]  
   [<InlineData(10.0, 50, 0.5, 0.3, 0.2)>]    
   let ``Assert GBMPath Expected Value`` (S0, endTime, dt, mu, sigma) =
        let expectation t = S0 * exp(mu * t / 365.0) // E[S_t] = S_0 * exp(mu * t)
        let expectedValues : float list =
            [for t in 0.0 .. dt .. float endTime -> expectation t]
        let n = 100_000
        let simulations : float list list =
            List.init n (fun _ -> GBMPath S0 endTime dt mu sigma)
        let simAvg : float list =
            List.transpose simulations
            |> List.map List.average
        let tolerance = 1e-2
        let test : bool = expectedValues
                         |> List.zip simAvg
                         |> List.forall (fun (ex, sim) -> abs ((ex - sim) / ex) <= tolerance)

        test |> should equal true
        
        