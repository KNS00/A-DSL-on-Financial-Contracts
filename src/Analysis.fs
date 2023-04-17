module Analysis
open Domain
open Evaluations
open Simulations
//open exchangeRates

/// <summary>
/// Runs a Monte Carlo simulation to estimate the expected value of a given contract.
/// Uses the given options underlying stocks to generate stock price before simulation.
/// </summary>
/// <param name="c1">The contract to simulate.</param>
/// <returns>The expected value of the option.</returns>
let simulateContract (c1 : Contract) : float =
    let sims = 10
    let E (s : string, t : int) : float = 
        let resultMap = makeE [s] t 1.0
        match resultMap |> Map.tryFind (s, t) with
        | Some value ->
            printfn "%s %A " "stock price" value
            value
        | None -> failwith "Stock was not found"
    let evaluations : float list =
        [for _ in 1..sims ->
            let res = evalc I E c1
            printfn "%s %A" "evaluation" res
            res]
    evaluations |> List.average


