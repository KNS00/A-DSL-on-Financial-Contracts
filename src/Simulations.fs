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
/// Calculates the interest rate for a given time period.
/// </summary>
/// <param name="i">The time period in days.</param>
/// <returns>The interest rate as a float.</returns>
let I (i : int) : float = 
    let yearlyInterestRate = 0.02 // assume 0.02
    exp(yearlyInterestRate * float(i)/365.0) //



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
let GeometricBrownianMotion (currentPrice : float, startTime : int, endTime : int, dt : float, mu: float, sigma: float, wpValues : float list) : float list =
    let t : float list = [float startTime .. dt .. float endTime]
    List.mapi (fun i _ -> currentPrice * exp((mu - 0.5 * sigma**2.0) * float t.[i] + sigma * wpValues.[i])) t
    // List.mapi (fun i _ -> currentPrice * exp((mu - 0.5 * sigma**2.0) * float t.[i] + sigma * sqrt(float t.[i]) * wpValues.[i])) t

/// <summary>
/// Simulates a list of stocks.
/// </summary>
/// <param name="stocks">A list of the names of the stocks to simulate.</param>
/// <param name="t">The time horizon for the simulation, in days.</param>
/// <param name="dt">The time step for the simulation.</param>
/// <param name="wpValues">A list of floats that represent a Wiener Process.</param>
/// <returns>A list of tuples, each containing a stock name and a list of simulated prices on the date t.</returns>
let simStocks (stocks : string list) (t : int) (dt : float) (wpValues : float list) : (string * (int * float) list) list =
    let simulate (currentPrice: float)  (mu : float) (sigma : float) (wpValues : float list) : (int * float) list = 
        let dates : int list = [0 .. int dt .. t]
        let GBM : float list = GeometricBrownianMotion(currentPrice, 0, t, dt, mu, sigma, wpValues)
        List.map2 (fun d p -> (d, p)) dates GBM
    let stockParameters = 
        stocks 
        |> List.choose (fun s -> XMLFunctions.getStockParameters s) 
    let sim : (string * (int * float) list) list = 
        stockParameters
        |> List.map (fun (S0, mu, sigma) -> (stocks |> List.find (fun s -> XMLFunctions.getStockParameters s = Some(S0, mu, sigma)), simulate S0 mu sigma wpValues)) 
    sim


/// <summary>
/// A single simulation of the given list of stocks for a given time period.
/// </summary>
/// <param name="stocks">A list of stocks to simulate.</param>
/// <param name="t">The number of days from the simulation start date to the simulation end date.</param>
/// <param name="dt">The size of the time steps for the simulation.</param>
/// <returns>A list of simulated stock prices for each stock in the list.</returns>
let mc1 (stocks : string list) (t : int) (dt : float) : float list =
    let wpValues = WienerProcess(0, t, dt)
    let data = simStocks stocks t dt wpValues
    let E (s, n) : float =
        let stockData = List.find (fun (s', _) -> s = s') data |> snd
        let quote = List.find (fun (n', _) -> n = n') stockData |> snd
        quote
    data |> List.map (fun (_, prices) -> List.map snd prices |> List.last)

/// <summary>
/// Evaluates the value of a stock at a given point in time.
/// </summary>
/// <param name="name">The name of the stock to evaluate.</param>
/// <param name="t">The point in time to evaluate the stock at.</param>
/// <returns>The value of the stock at the time as a float.</returns>
let rec E(name: string, t: int) : float =  
  match XMLFunctions.getPrice name t with
  | Some price -> price
  | None ->
      //List.head (stockSimulations.mc [name] 100_000 t 1.0)
    List.head (mc1 [name] t 1.0)


(*
let simulateStocks (o : Obs list) (stocks : string list) (t : int) (dt : float): float list =
    let sims = 100_000
    let simulate () : float list = 
        let rec loop (i: int) (acc : float list) : float list =
            if i = sims then acc
            else 
                let simulationResults = mc1 stocks t dt
                let updatedAcc = List.map2 (+) acc simulationResults
                loop (i + 1) updatedAcc
        let totalSimulatedPrices = loop 0 (List.init (List.length stocks) (fun _ -> 0.0))
        let averageSimulatedPrices = List.map (fun x -> x / float sims) totalSimulatedPrices
        averageSimulatedPrices
    let averagePrices = simulate ()
    List.iter2 (fun stockName avgPrice -> XMLFunctions.updateStockData stockName t avgPrice) stocks averagePrices
    averagePrices
    *)





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
