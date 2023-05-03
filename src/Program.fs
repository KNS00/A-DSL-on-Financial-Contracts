module Program
open Domain
open Analysis
open Evaluations
open Simulations
open FSharp.Data
open XMLFunctions // Import XMLFunctions module
open XPlot.Plotly
open NUnit.Framework
open FsUnit
let main args =
    //let callValue = simulateContract exampleEuropeanCallOption
    //let putValue = simulateContract exampleEuropeanPutOption
    //let parity = callValue - putValue - (130.0 - 100.0 * exp(-0.02*30.0))

    //let contractValue = simulateContract newContract
    //let dikuA = Underlying("DIKU A/S", 10)
    //let dikuB = Underlying("DIKU A/S", 5)
    //let contracts = All([Scale(dikuA, One USD); Scale(dikuB, One USD)])
    //let sim : float = simulateContract contracts
    //let stockObs (c : Contract List) : (Obs List) = List.concat (List.map getStocksAsObs c)
    //let result = stockObs [exampleEuropeanCallOption; exampleEuropeanPutOption; exampleForward]
    //let realresult = List.map getUnderlyingInfo result |> List.distinct
    //printfn "%A" callValue
    //printfn "%A" putValue
    //printfn "%s %A" "parity" parity
    //printfn "%A" (testParity (exampleEuropeanCallOption, exampleEuropeanPutOption, 100.0))

    let startTime = 0
    let endTime = 10
    let dt = 1.0
    let volatility = 0.2
    let drift = 0.05
    let wpValues = WienerProcess(startTime, endTime, dt)
    let gbmValues = GeometricBrownianMotion(100.0, startTime, endTime, dt, 0.05, 0.2, wpValues)

    let plotWienerProcess = 
        Scatter(
            x = [float startTime .. float endTime],
            y = wpValues,
            mode = "lines",
            name = "Wiener Process"
        )

    let plotGBM = 
        Scatter(
            x = [float startTime .. float endTime],
            y = gbmValues,
            mode = "lines",
            name = "Geometric Brownian Motion"
        )

    let strikePrice = 110.0
    let callOptionPayoff = gbmValues |> List.map (fun price -> max (price - strikePrice) 0.0)

    let plotCallOptionPayoff = 
        Scatter(
            x = [float startTime .. float endTime],
            y = callOptionPayoff,
            mode = "lines",
            name = "Call Option Payoff"
        )

    let layout = Layout(title = "Wiener Process, Geometric Brownian Motion, and Call Option Payoff")
    let chart = Chart.Plot([|plotWienerProcess; plotGBM; plotCallOptionPayoff|], layout)
    //chart.Show()
    0.0
main [] |> ignore