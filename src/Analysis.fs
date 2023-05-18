module Analysis
open Domain
open Evaluations
open Simulations
open XMLFunctions
//open exchangeRates

/// <summary>
/// Runs a Monte Carlo simulation to estimate the expected value of a given contract.
/// Uses the given options underlying stocks to generate stock price before simulation.
/// </summary>
/// <param name="c1">The contract to simulate.</param>
/// <returns>The expected value of the option.</returns>
let simulateContract (sims : int) (c : Contract) : float =
    let underlyings : string list = getStocks(c)
    let maturity = getMaturityDate(c)
    let evaluations : float list =
        [for _ in 1..sims ->
            let resultMap = makeE underlyings maturity 1.0
            let E(s,t) : float = Map.find(s, t) resultMap
            let res = evalc I E c
            //printfn "%A" res
            res]
    //printfn "%s %A" "evaluations" evaluations
    //printfn "%s %A" "sum of evaluations" (List.sum evaluations)
    evaluations |> List.average

