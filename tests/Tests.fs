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
    [<InlineData(10.0, 10.0)>]      // Value 10.0 = 10.0
    [<InlineData(50.0, 50.0)>]      // Value 50.0 = 50.0
    [<InlineData(0.0, 0.0)>]        // Value 0.0 = 0.0
    [<InlineData(-10.0, -10.0)>]    // Value -10.0 = -10.0
    [<InlineData(100.0, 100.0)>]    // Value 100.0 = 100.0
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
    [<InlineData(0.0, 0.0, 0.0)>]       // 0.0 * 0.0 = 0.0
    [<InlineData(1.0, 0.0, 0.0)>]       // 1.0 * 0.0 = 0.0
    [<InlineData(0.0, 1.0, 0.0)>]       // 0.0 * 1.0 = 0.0
    [<InlineData(0.5, 0.5, 0.25)>]      // 0.5 * 0.5 = 0.25
    [<InlineData(3.0, 4.0, 12.0)>]      // 3.0 * 4.0 = 12.0
    [<InlineData(10.0, -10.0, -100.0)>] // 10.0 * -10.0 = -100.0
    [<InlineData(-10.0, 10.0, -100.0)>] // -10.0 * 10.0 = -100.0
    [<InlineData(-5.0, -5.0, 25.0)>]    // -5.0 * -5.0 = 25.0
    [<InlineData(-1.0, -1.0, 1.0)>]     // -1.0 * -1.0 = 1.0
    [<InlineData(1.5, 2.5, 3.75)>]      // 1.5 * 2.5 = 3.75
    [<InlineData(2.0, 2.0, 4.0)>]       // 2.0 * 2.0 = 4.0
    [<InlineData(0.0, 10.0, 10.0)>]     // 0.0 * 10.0 = 0.0
    [<InlineData(10.0, 0.0, 0.0)>]      // 10.0 * 0.0 = 0.0
    [<InlineData(2.5, 2.5, 6.25)>]      // 2.5 * 2.5 = 6.25
    let ``evalo with Mul``(input : float * float, expectedOutput) =
        let (obs1, obs2) : Obs * Obs = (Value (fst input), Value (snd input))
        evalo E (Mul (obs1, obs2)) |> should equal expectedOutput

    [<Theory>]
    [<InlineData(10.0, 10.0, 20.0)>]    // 10.0 + 10.0 = 20.0
    [<InlineData(0.0, 0.0, 0.0)>]       // 0.0 + 0.0 = 0.0
    [<InlineData(1.0, 0.0, 1.0)>]       // 1.0 + 0.0 = 1.0
    [<InlineData(0.0, 1.0, 1.0)>]       // 0.0 + 1.0 = 1.0
    [<InlineData(0.5, 0.5, 1.0)>]       // 0.5 + 0.5 = 1.0
    [<InlineData(3.0, 4.0, 7.0)>]       // 3.0 + 4.0 = 7.0
    [<InlineData(10.0, -10.0, 0.0)>]    // 10.0 + -10.0 = 0.0
    [<InlineData(-10.0, 10.0, 0.0)>]    // -10.0 + 10.0 = 0.0
    [<InlineData(-5.0, -5.0, -10.0)>]   // -5.0 + -5.0 = -10.0
    [<InlineData(-1.0, -1.0, -2.0)>]    // -1.0 + -1.0 = -2.0
    [<InlineData(1.5, 2.5, 4.0)>]       // 1.5 + 2.5 = 4.0
    [<InlineData(2.0, 2.0, 4.0)>]       // 2.0 + 2.0 = 4.0
    [<InlineData(0.0, 10.0, 10.0)>]     // 0.0 + 10.0 = 10.0
    [<InlineData(10.0, 0.0, 10.0)>]     // 10.0 + 0.0 = 10.0
    [<InlineData(2.5, 2.5, 5.0)>]       // 2.5 + 2.5 = 5.0
    let ``evalo with Add``(input : float * float, expectedOutput) =
        let (obs1, obs2) : Obs * Obs = (Value (fst input), Value (snd input))
        evalo E (Add (obs1, obs2)) |> should equal expectedOutput

    [<Theory>]
    [<InlineData(10.0, 10.0, 0.0)>]     // 10.0 - 10.0 = 0.0
    [<InlineData(0.0, 0.0, 0.0)>]       // 0.0 - 0.0 = 0.0
    [<InlineData(1.0, 0.0, 1.0)>]       // 1.0 - 0.0 = 1.0
    [<InlineData(0.0, 1.0, -1.0)>]      // 0.0 - 1.0 = -1.0
    [<InlineData(0.5, 0.5, 0.0)>]       // 0.5 - 0.5 = 0.0
    [<InlineData(3.0, 4.0, -1.0)>]      // 3.0 - 4.0 = -1.0
    [<InlineData(10.0, -10.0, 20.0)>]   // 10.0 - (-10.0) = 20.0
    [<InlineData(-10.0, 10.0, -20.0)>]  // (-10.0) - 10.0 = -20.0
    [<InlineData(-5.0, -5.0, 0.0)>]     // (-5.0) - (-5.0) = 0.0
    [<InlineData(-1.0, -1.0, 0.0)>]     // (-1.0) - (-1.0) = 0.0
    [<InlineData(1.5, 2.5, -1.0)>]      // 1.5 - 2.5 = -1.0
    [<InlineData(2.0, 2.0, 0.0)>]       // 2.0 - 2.0 = 0.0
    [<InlineData(0.0, 10.0, -10.0)>]    // 0.0 - 10.0 = -10.0
    [<InlineData(10.0, 0.0, 10.0)>]     // 10.0 - 0.0 = 10.0
    [<InlineData(2.5, 2.5, 0.0)>]       // 2.5 - 2.5 = 0.0
    let ``evalo with Sub``(input : float * float, expectedOutput) =
        let (obs1, obs2) : Obs * Obs = (Value (fst input), Value (snd input))
        evalo E (Sub (obs1, obs2)) |> should equal expectedOutput


    [<Theory>]
    [<InlineData(10.0, 10.0, 10.0)>]    // max(10.0, 10.0) = 10.0
    [<InlineData(0.0, 0.0, 0.0)>]       // max(0.0, 0) = 0.0
    [<InlineData(1.0, 0.0, 1.0)>]       // max(1.0, 0.0) = 1.0
    [<InlineData(0.0, 1.0, 1.0)>]       // max(0.0, 1.0) = 1.0
    [<InlineData(0.5, 0.5, 0.5)>]       // max(0.5, 0.5) = 0.5
    [<InlineData(3.0, 4.0, 4.0)>]       // max(3.0, 4.0) = 4.0
    [<InlineData(10.0, -10.0, 10.0)>]   // max(10.0, -10.0) = 10.0
    [<InlineData(-10.0, 10.0, 10.0)>]   // max(-10.0, 10.0) = 10.0
    [<InlineData(-5.0, -5.0, -5.0)>]    // max(-5.0, -5.0) = -5.0
    [<InlineData(-1.0, -1.0, -1.0)>]    // max(-1.0, -1.0) = -1.0
    [<InlineData(1.5, 2.5, 2.5)>]       // max(1.5, 2.5) = 2.5
    [<InlineData(0.0, 10.0, 10.0)>]     // max(0.0, 10.0) = 10.0
    [<InlineData(10.0, 0.0, 10.0)>]     // max(10.0, 0.0) = 10.0
    let ``evalo with Max``(input : float * float, expectedOutput) =
        let (obs1, obs2) : Obs * Obs = (Value (fst input), Value (snd input))
        evalo E (Max (obs1, obs2)) |> should equal expectedOutput


module evalcTests =
    [<Theory>]
    let ``evalc with One``(input : Contract, expectedOutput) =
        0.0

module testSimulations =
    
    let ``discount function should correctly discount back the value``(input, expectedOutput) =
        0.0