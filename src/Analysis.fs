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
let simulateContract (c : Contract) : float =
    let sims = 100_000 /// change this to input variable
    let underlyings : string list = getStocks(c)
    let price : float = getPrice (List.head underlyings) 0
    printfn "%s %A" "underlying price:" price
    let maturity = getMaturityDate(c)
    let evaluations : float list =
        [for _ in 1..sims ->
            let resultMap = makeE underlyings maturity 1.0
            let E(s,t) : float = Map.find(s, t) resultMap
            printfn "%A" resultMap
            printfn "%A" (E("DIKU A/S", 30))

            let res = evalc I E c
            printfn "%A" res
            res]
    printfn "%s %A" "evaluations" evaluations
    printfn "%s %A" "sum of evaluations" (List.sum evaluations)
    evaluations |> List.average

