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


/// <summary>
/// Discounts the value 1 back in time according to the interest rate.
/// </summary>
/// <param name="i">The time period in days.</param>
/// <returns>The value 1 discounted to the current time.</returns>
let I (r : float) (t : int) : float =  
    let dailyInterestRate : float = r / 365.0 // assume 0.02 for now
    let presentValue = 1.0/((1.0+dailyInterestRate)**float t)
    presentValue

/// <summary>
/// Generates a Wiener Process of normally distributed random numbers.
/// </summary>
/// <param name="startTime">The start time of the Wiener Process.</param>
/// <param name="endTime">The end time of the Wiener Process.</param>
/// <param name="dt">The time step for the Wiener Process.</param>
/// <returns>A list of floats represent the Wiener Process.</returns>
let WienerProcess(startTime : int, endTime : int, dt : float) : float list = 
    let normalDistribution = ContinuousDistribution.normal 0.0 1.0
    let numSteps = int(ceil((float(endTime) - float(startTime)) / dt))
    let sampleValues = List.init numSteps (fun _ -> normalDistribution.Sample() * sqrt(dt))
    let results = List.scan (+) 0.0 sampleValues
    results

/// <summary>
/// Simulates a Geometric Brownian Motion from start time to end time.
/// </summary>
/// <param name="currentPrice">The current price of the stock.</param>
/// <param name="startTime">The start time of the simulation.</param>
/// <param name="endTime">The end time of the simulation.</param>
/// <param name="dt">The time step for the simulation.</param>
/// <param name="mu">The drift parameter for the simulation.</param>
/// <param name="sigma">The volatility parameter for the simulation.</param>
/// <param name="wpValues">A list of floats that represent a Wiener Process.</param>
/// <returns>A list of floats that represent the Geometric Brownian Motion.</returns>
let GeometricBrownianMotion (currentPrice : float, startTime : int, endTime : int, dt : float, mu: float, sigma : float, wpValues : float list) : float list =
    let t : float list = [float startTime .. dt .. float endTime]
    let output (i : int) = currentPrice * exp(((mu - 0.5 * (sigma**2.0)) * t.[i]) + sigma * wpValues.[i])
    List.mapi (fun i _ -> output i) t

/// <summary>
/// Simulates a stock from now until the given time based on the current price, drift and volatility found in a XML file.
/// </summary>
/// <param name="stock">The stock to be simulated.
/// <param name="t">The end time of the simulation.
/// <param name="dt">The time increment.
/// <returns>A tuple list containing the time and price of the stock.
let simStock (stock : string) (t : int) (dt : float) : (int * float) list =
    let wpValues = WienerProcess(0, t, dt)
    let simulate (currentPrice: float)  (mu : float) (sigma : float) (wpValues : float list) : (int * float) list = 
        let dates : int list = [0 .. int dt .. t]
        let GBM : float list = GeometricBrownianMotion(currentPrice, 0, t, dt, mu, sigma, wpValues)
        List.map2 (fun d p -> (d, p)) dates GBM
    match XMLFunctions.getStockParameters stock with 
    | Some (S0, mu, sigma) -> simulate S0 mu sigma wpValues
    | None -> failwith "Stock was not found"
    

/// <summary>
/// A single simulation of the given list of stocks for a given time period.
/// </summary>
/// <param name="stocks">A list of stocks to simulate.</param>
/// <param name="t">The number of days from the simulation start date to the simulation end date.</param>
/// <param name="dt">The size of the time steps for the simulation.</param>
/// <returns>A list of simulated stock prices for each stock in the list.</returns>
let makeE (stocks : string list) (t : int) (dt : float) : Map<(string * int), float> =
    let data =
        stocks
        |> List.collect (fun (s : string) -> // Collect is the same as map but where we flatten the list afterwards.
            let stockData = simStock s t dt
            stockData |> List.map (fun (i : int, f : float) -> ((s, i), f)) 
        )
    data |> Map.ofList




(*
// Define the parameters
let currentPrice = 100.0
let startTime = 0
let endTime = 10
let dt = 1.0
let mu = 1.0
let sigmaValues = [| 0.2; 0.4; 0.6; 0.8; 1.0 |]

// Generate the Wiener process values
let wpValues = WienerProcess(startTime, endTime, dt)

// Generate the GBM values for each sigma value
let gbmValues = 
    sigmaValues 
    |> Array.map (fun sigma -> GeometricBrownianMotion(currentPrice, startTime, endTime, dt, mu, sigma, wpValues))

// Create the plot traces
let traces =
    gbmValues
    |> Array.mapi (fun i values -> 
        Scatter(
            x = [ float startTime .. dt  .. float endTime ],
            y = values,
            mode = "lines",
            name = sprintf "GBM (sigma = %f)" sigmaValues.[i]
        )
    )

// Create the plot
let plot = Chart.Plot traces

// Set plot options
plot
|> Chart.WithTitle "Geometric Brownian Motion"
|> Chart.WithXTitle "Time"
|> Chart.WithYTitle "Price"
|> Chart.WithLegend true
*)





