module Program
open Domain
open Evaluations
open Simulations
open Plots
open Instruments
open Management
open FSharp.Stats

// A function used to replicate the results of the report
let rec replicateResults(z : string) =
    let set_seed (seed : int) = Random.SetSampleGenerator(Random.RandThreadSafe(seed))
    match z with
    | "section5" -> 
        set_seed(1) 
        printfn "Running results for section 5..."
        printfn "The plots will be opened in a browser tab. Please open your browser."
        plotWienerProcess()
        plotAsian()
        printfn "Simulating the price of the call..."
        let ec1 = europeanCall2 10 "AAPL" 95. USD

        printfn "%A" (simulateContract 100_000 ec1)
    | "section6" -> printfn "To run the tests from section 6, please check the README file."
    | "section7" ->
        set_seed(1)
        printfn "Running results for section 7..."
        let E(s,t) : float =
            match (s,t) with
            | ("AAPL", 0) -> 100.0
            | _-> failwith "price not found"

        let f (cur : Currency) : float =
            match cur with
            | USD -> 1.0
            | EUR -> 1.10
            | GBP -> 1.24
            | DKK -> 0.15
        let c = All[
            europeanCall2 1 "AAPL" 90.0 USD
            zcb 1 500.0 USD
            chooserOption 1 10 "AAPL" 120.0 USD
            ]
        let c1 = advance E 1 c
        let c2 = choose (evalc f I E) c1
        printfn "Contract before advancement:\n %A" c
        printfn "Contract after advancement:\n %A" c1
        printfn "Contract after choice and advancement:\n %A" c2
        printfn "The value of the portfolio:\n %A" (simulateContract 10_000 c2)
    | "" ->
        printfn "No argument given. Running results for section 5 and 7..."
        replicateResults("section5")
        replicateResults("section7")
    | _ -> failwith "Argument not found. Possible arguments are section5, section7 or no argument."

[<EntryPoint>]
let main (argv : string[]) =
    if Array.isEmpty argv then
        replicateResults ""
        let E(s,t) : float =
            match (s,t) with
            | ("AAPL", 0) -> 100.0
            | _-> failwith "price not found"

        let f (cur : Currency) : float =
            match cur with
            | USD -> 1.0
            | EUR -> 1.10
            | GBP -> 1.24
            | DKK -> 0.15
        let c = chooserOption 1 10 "AAPL" 120.0 USD
        let c1 = advance E 1 c
        printfn "%A" c1
        printfn "%A" (choose (simulateContract 100_000) c1)

    else
        Array.map (fun x -> replicateResults x) argv |> ignore
    0