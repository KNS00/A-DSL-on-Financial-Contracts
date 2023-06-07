module Simulations
open FSharp.Stats
open FSharp.Math
open FSharp.Stats.Distributions
open FSharp.Data
open XPlot.Plotly
open System.IO
open System.Runtime.Serialization.Formatters.Binary
open System.Collections.Generic
open FSharp.Data
open Domain
open Evaluations
open Management
open Examples

let normal = ContinuousDistribution.normal 0.0 1.0

let f (c : Currency) : float =
    match c with
    | USD -> 1.0
    | EUR -> 1.10
    | GBP -> 1.24
    | DKK -> 0.15

/// <summary>
/// Discounts the value 1 back in time according to the interest rate.
/// </summary>
/// <param name="i">The time period in days.</param>
/// <returns>The value 1 discounted to the current time.</returns>
let I (t : int) : float =  
    let r = 0.02
    let presentValue : float = exp(-r/365. * (float t)) // e^{-rt}
    presentValue

/// <summary>
/// Generates a Wiener Process of normally distributed random numbers.
/// </summary>
/// <param name="T">The end time of the Wiener Process.</param>
/// <param name="dt">The time step for the Wiener Process.</param>
/// <returns>A list of floats represent the Wiener Process.</returns>
let WienerProcess(T : int) (dt : float) : float list =
    let numSteps = int(ceil((float T) / dt))
    let sampleValues = List.init numSteps (fun _ -> normal.Sample() * sqrt(dt))
    let results = List.scan (+) 0.0 sampleValues 
    results


let GBM (initialPrice : float) (T : int) (mu : float) (sigma : float) : float =
    let Td = float T/365.
    let drift = (mu - 0.5 * (sigma ** 2.0)) * float Td
    let diffusion = sigma * sqrt(float Td) * normal.Sample()
    initialPrice * exp(drift + diffusion)


/// <summary>
/// Simulates a Geometric Brownian Motion from start time to end time.
/// </summary>
/// <param name="currentPrice">The current price of the stock.</param>
/// <param name="T">The end time of the simulation.</param>
/// <param name="dt">The time step for the simulation.</param>
/// <param name="mu">The drift parameter for the simulation.</param>
/// <param name="sigma">The volatility parameter for the simulation.</param>
/// <param name="wpValues">A list of floats that represent a simulated Wiener Process.</param>
/// <returns>A list of floats that represent the Geometric Brownian Motion.</returns>
let GBMPath (initialPrice: float) (T: int) (dt: float) (mu: float) (sigma: float) : float list =
    let numSteps = int (float T / dt)
    let dt_ = dt / 365.0
    let drift = (mu - 0.5 * sigma**2.) * dt_
    let diffusion = sigma * sqrt(dt_)

    let simulateSteps (initialPrice: float) : float list =
        let Z = List.init numSteps (fun _ -> normal.Sample() * sqrt(dt_))
        let prices = List.scan (fun price dW -> price * exp(drift + diffusion * dW)) initialPrice Z
        prices

    simulateSteps initialPrice

/// <summary>
/// Simulates a stock from now until the given time based on the current price, drift and volatility found in a XML file.
/// </summary>
/// <param name="stock">The stock to be simulated.
/// <param name="t">The end time of the simulation.
/// <returns>the price of the stock. </returns>
let simStock (stock : string) (t : int) : float =
    match XMLFunctions.getStockParameters stock with
    | Some (S0, mu, sigma) -> GBM S0 t mu sigma 
    | None -> failwith "Stock was not found"
    

/// <summary>
/// A single simulation of the given list of stocks for a given time period.
/// </summary>
/// <param name="stocks">A list of stocks to simulate.</param>
/// <param name="t">The number of days from the simulation start date to the simulation end date.</param>
/// <param name="dt">The size of the time steps for the simulation.</param>
/// <returns>A list of simulated stock prices for each stock in the list.</returns>
let makeE (stocks : (string * int) list) : Map<(string * int), float> =
    let keyValuePairs =
        stocks
        |> List.map (fun (stock, t) -> ((stock, t), simStock stock t))
    let map = Map.ofSeq keyValuePairs
    map

/// <summary>
/// Runs a Monte Carlo simulation to estimate the expected value of a given contract.
/// Uses the given options underlying stocks to generate stock price before simulation.
/// </summary>
/// <param name="c1">The contract to simulate.</param>
/// <returns>The expected value of the option.</returns>

let simulateContract (sims : int) (c : Contract) : float =
    let underlyings : (string * int) list = underlyings c
    let evaluations : float list =
        [for _ in 1..sims ->
            let resultMap = makeE underlyings
                                  
            let E(s,t) : float = Map.find(s, t) resultMap
            let res = evalc f I E c
            //printfn "%A" res
            res]
    //printfn "%s %A" "evaluations" evaluations
    //printfn "%s %A" "sum of evaluations" (List.sum evaluations)
    evaluations |> List.average

