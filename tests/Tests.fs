module Tests
open Domain
open Evaluations
open Simulations
open Analysis
open FsUnit
open Xunit


module evaloTests = 
    let E (s: string, t: int) = // test E function: for testing, returns 0.0 if the stock is not found
        match s, t with
        | "AAPL", 1 -> 100.23
        | "AAPL", 3 -> 104.33
        | "AAPL", 5 -> 108.11
        | "GOOG", 1 -> 200.54
        | "GOOG", 5 -> 216.34
        | "MSFT", 2 -> 23.03
        | _ -> 0.0

    [<Theory>]
    [<InlineData(10.0, 10.0)>]      // Value 10.0   = 10.0
    [<InlineData(50.0, 50.0)>]      // Value 50.0   = 50.0
    [<InlineData(0.0, 0.0)>]        // Value 0.0    = 0.0
    [<InlineData(-10.0, -10.0)>]    // Value -10.0  = -10.0
    [<InlineData(100.0, 100.0)>]    // Value 100.0  = 100.0
    let ``evalo with Value should return the value itself``(input, expectedOutput) =
        evalo E (Value input) |> should equal expectedOutput

    [<Theory>]
    [<InlineData("AAPL", 1)>]   // Underlying("AAPL", 1)    = E("AAPL", 1) 
    [<InlineData("DIKU", 3)>]   // Underlying("DIKU", 3)    = E("DIKU", 3) 
    [<InlineData("GOOG", -17)>] // Underlying("GOOG", -17)  = E("GOOG", -17) 
    [<InlineData("GOOG", 1)>]   // Underlying("GOOG", 1)    = E("DIKU", 1) 
    [<InlineData("MSFT", 2)>]   // Underlying("MSFT", 2)    = E("MSFT", 2)
    [<InlineData("AAPL", 3)>]   // Underlying("AAPL", 3)    = E("AAPL", 3)
    [<InlineData("AAPL", 5)>]   // Underlying("AAPL", 5)    = E("DIKU", 5)
    [<InlineData("AAPL", 6)>]   // Underlying("DIKU", 6)    = E("DIKU", 6)
    let ``evalo with Underlying should return the output of E``(input : (string * int)) =
        let expectedOutput = E(input)
        evalo E (Underlying input) |> should equal expectedOutput



    [<Theory>]
    [<InlineData(0.0, 0.0, 0.0)>]       // 0.0 * 0.0    = 0.0
    [<InlineData(1.0, 0.0, 0.0)>]       // 1.0 * 0.0    = 0.0
    [<InlineData(0.0, 1.0, 0.0)>]       // 0.0 * 1.0    = 0.0
    [<InlineData(0.5, 0.5, 0.25)>]      // 0.5 * 0.5    = 0.25
    [<InlineData(3.0, 4.0, 12.0)>]      // 3.0 * 4.0    = 12.0
    [<InlineData(10.0, -10.0, -100.0)>] // 10.0 * -10.0 = -100.0
    [<InlineData(-10.0, 10.0, -100.0)>] // -10.0 * 10.0 = -100.0
    [<InlineData(-5.0, -5.0, 25.0)>]    // -5.0 * -5.0  = 25.0
    [<InlineData(-1.0, -1.0, 1.0)>]     // -1.0 * -1.0  = 1.0
    [<InlineData(1.5, 2.5, 3.75)>]      // 1.5 * 2.5    = 3.75
    [<InlineData(2.0, 2.0, 4.0)>]       // 2.0 * 2.0    = 4.0
    [<InlineData(0.0, 10.0, 10.0)>]     // 0.0 * 10.0   = 0.0
    [<InlineData(10.0, 0.0, 0.0)>]      // 10.0 * 0.0   = 0.0
    [<InlineData(2.5, 2.5, 6.25)>]      // 2.5 * 2.5    = 6.25
    let ``evalo with Mul``(input : float * float, expectedOutput) =
        let (obs1, obs2) : Obs * Obs = (Value (fst input), Value (snd input))
        evalo E (Mul (obs1, obs2)) |> should equal expectedOutput

    [<Theory>]
    [<InlineData(10.0, 10.0, 20.0)>]    // 10.0 + 10.0  = 20.0
    [<InlineData(0.0, 0.0, 0.0)>]       // 0.0 + 0.0    = 0.0
    [<InlineData(1.0, 0.0, 1.0)>]       // 1.0 + 0.0    = 1.0
    [<InlineData(0.0, 1.0, 1.0)>]       // 0.0 + 1.0    = 1.0
    [<InlineData(0.5, 0.5, 1.0)>]       // 0.5 + 0.5    = 1.0
    [<InlineData(3.0, 4.0, 7.0)>]       // 3.0 + 4.0    = 7.0
    [<InlineData(10.0, -10.0, 0.0)>]    // 10.0 + -10.0 = 0.0
    [<InlineData(-10.0, 10.0, 0.0)>]    // -10.0 + 10.0 = 0.0
    [<InlineData(-5.0, -5.0, -10.0)>]   // -5.0 + -5.0  = -10.0
    [<InlineData(-1.0, -1.0, -2.0)>]    // -1.0 + -1.0  = -2.0
    [<InlineData(1.5, 2.5, 4.0)>]       // 1.5 + 2.5    = 4.0
    [<InlineData(2.0, 2.0, 4.0)>]       // 2.0 + 2.0    = 4.0
    [<InlineData(0.0, 10.0, 10.0)>]     // 0.0 + 10.0   = 10.0
    [<InlineData(10.0, 0.0, 10.0)>]     // 10.0 + 0.0   = 10.0
    [<InlineData(2.5, 2.5, 5.0)>]       // 2.5 + 2.5    = 5.0
    let ``evalo with Add``(input : float * float, expectedOutput) =
        let (obs1, obs2) : Obs * Obs = (Value (fst input), Value (snd input))
        evalo E (Add (obs1, obs2)) |> should equal expectedOutput

    [<Theory>]
    [<InlineData(10.0, 10.0, 0.0)>]     // 10.0 - 10.0      = 0.0
    [<InlineData(0.0, 0.0, 0.0)>]       // 0.0 - 0.0        = 0.0
    [<InlineData(1.0, 0.0, 1.0)>]       // 1.0 - 0.0        = 1.0
    [<InlineData(0.0, 1.0, -1.0)>]      // 0.0 - 1.0        = -1.0
    [<InlineData(0.5, 0.5, 0.0)>]       // 0.5 - 0.5        = 0.0
    [<InlineData(3.0, 4.0, -1.0)>]      // 3.0 - 4.0        = -1.0
    [<InlineData(10.0, -10.0, 20.0)>]   // 10.0 - (-10.0)   = 20.0
    [<InlineData(-10.0, 10.0, -20.0)>]  // (-10.0) - 10.0   = -20.0
    [<InlineData(-5.0, -5.0, 0.0)>]     // (-5.0) - (-5.0)  = 0.0
    [<InlineData(-1.0, -1.0, 0.0)>]     // (-1.0) - (-1.0)  = 0.0
    [<InlineData(1.5, 2.5, -1.0)>]      // 1.5 - 2.5        = -1.0
    [<InlineData(2.0, 2.0, 0.0)>]       // 2.0 - 2.0        = 0.0
    [<InlineData(0.0, 10.0, -10.0)>]    // 0.0 - 10.0       = -10.0
    [<InlineData(10.0, 0.0, 10.0)>]     // 10.0 - 0.0       = 10.0
    [<InlineData(2.5, 2.5, 0.0)>]       // 2.5 - 2.5        = 0.0
    let ``evalo with Sub``(input : float * float, expectedOutput) =
        let (obs1, obs2) : Obs * Obs = (Value (fst input), Value (snd input))
        evalo E (Sub (obs1, obs2)) |> should equal expectedOutput


    [<Theory>]
    [<InlineData(10.0, 10.0, 10.0)>]    // max(10.0, 10.0) = 10.0
    [<InlineData(0.0, 0.0, 0.0)>]       // max(0.0, 0)      = 0.0
    [<InlineData(1.0, 0.0, 1.0)>]       // max(1.0, 0.0)    = 1.0
    [<InlineData(0.0, 1.0, 1.0)>]       // max(0.0, 1.0)    = 1.0
    [<InlineData(0.5, 0.5, 0.5)>]       // max(0.5, 0.5)    = 0.5
    [<InlineData(3.0, 4.0, 4.0)>]       // max(3.0, 4.0)    = 4.0
    [<InlineData(10.0, -10.0, 10.0)>]   // max(10.0, -10.0) = 10.0
    [<InlineData(-10.0, 10.0, 10.0)>]   // max(-10.0, 10.0) = 10.0
    [<InlineData(-5.0, -5.0, -5.0)>]    // max(-5.0, -5.0)  = -5.0
    [<InlineData(-1.0, -1.0, -1.0)>]    // max(-1.0, -1.0)  = -1.0
    [<InlineData(1.5, 2.5, 2.5)>]       // max(1.5, 2.5)    = 2.5
    [<InlineData(0.0, 10.0, 10.0)>]     // max(0.0, 10.0)   = 10.0
    [<InlineData(10.0, 0.0, 10.0)>]     // max(10.0, 0.0)   = 10.0
    let ``evalo with Max``(input : float * float, expectedOutput) =
        let (obs1, obs2) : Obs * Obs = (Value (fst input), Value (snd input))
        evalo E (Max (obs1, obs2)) |> should equal expectedOutput

module evalcTests =
    0.0 |> ignore


module simulationTests =
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
    [<Theory>] 
    [<InlineData(5, 0.9997260724)>]    // 1/((1+0,02/365))^5)    = 0.9997260724
    [<InlineData(20, 0.9989047398)>]   // 1/((1+0,02/365))^20)   = 0.9989047398
    [<InlineData(30, 0.9983575597)>]   // 1/((1+0,02/365))^30)   = 0.9983575597
    [<InlineData(365, 0.9801992104)>]  // 1/((1+0,02/365))^365)  = 0.9801992104
    [<InlineData(1000, 0.9466810723)>] // 1/((1+0,02/365))^1000) = 0.9466810723
    let ``discount function should correctly discount back the value``(input : int, expectedOutput) =
        let tolerance = 1e-7
        let output = I input
        let isEqual = abs (output - expectedOutput) <= tolerance
        isEqual |> should equal true
    
    [<Theory>] 
    [<InlineData(0, 1, 1.0)>]
    [<InlineData(10, 15, 1.0)>]    
    [<InlineData(7, 10, 0.5)>]    
    [<InlineData(10, 15, 0.37)>]    
    [<InlineData(1, 10, 1.53)>]    
    [<InlineData(5, 10, 1.35)>]    
    let ``Wiener Process: E[W(t)] = 0 at any time.``(startTime, endTime, dt) =
        let simulation : float list =
            let n = 1_000_000
            [1..n] |> List.map (fun _ -> WienerProcess(startTime, endTime, dt))
            |> List.transpose
            |> List.map List.average
        let tolerance = 1e-2
        let isZero : bool = simulation |> List.forall (fun x ->
            if abs(x) > tolerance then
                printfn "%A" (abs(x))
                printfn "%A" (abs(x) <= tolerance)
            abs(x) <= tolerance)
        isZero |> should equal true
        
        
    [<Theory>] 
    [<InlineData(100.0, 0, 1, 0.1, 1.0, 0.2)>]     // S_0 = 100.0, start = 0, end = 1, dt = 0.1, mu 1.0, sigma = 0.2
    [<InlineData(15.0, 0, 10, 1.0, -0.2, 0.4)>]     // S_0 = 15.0, start = 0 end = 10, dt = 1.0, mu = 0.5, sigma = 0.4
    //[<InlineData(50.0, 10, 15, 1.0, 0.1, 0.1)>]  // S_0 = 50.0, start = 10, end = 11, dt = 0.01, mu = -0.5, sigma = 0.1
    [<InlineData(1.0, 0, 15, 1.0, 1.0, 0.2)>]      // S_0 = 1.0, start = 0, end = 3, dt = 0.07, mu = 0.0, sigma = 0.6

    let ``Assert GBM Expected Value``(S0, startTime, endTime, dt, mu, sigma) =
        let expectation t = S0 * exp(mu * t) // E[S_t] = S_0 * exp(mu * t)
        let expectedValues : float list =
            [for t in float startTime.. dt .. float endTime -> expectation t]
        let simulation : float list = // GBM Simulations
            let n = 2_000_000
            [1..n] |> List.map (fun _ ->
                let wpValues = WienerProcess(startTime , endTime, dt)
                GeometricBrownianMotion(S0, startTime, endTime, dt, mu, sigma, wpValues))
            |> List.transpose
            |> List.map List.average
        printfn "%s %A" "dt values:" [float startTime ..dt..float endTime]
        printfn "%s %A" "simulated values:" simulation
        printfn "%s %A" "expected values:" expectedValues
        let tolerance = 1e-2
        let test : bool = listsAreEqual expectedValues simulation tolerance
        test |> should equal true

       
       
       


       