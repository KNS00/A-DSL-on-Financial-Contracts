module Plots
// Project-specific modules
open Domain
open XMLFunctions
open Evaluations
open Simulations
open Analysis
open Examples


// External modules
open System
open FSharp.Stats
open FSharp.Stats.Distributions
open XPlot.Plotly




// plotting a Wiener Process
let plotWienerProcess() =
    let startTime = 0
    let endTime = 10
    let dt = 0.01
    let wpValues = WienerProcess(startTime, endTime, dt)
    let plotWienerProcess =
        let xValues = [float startTime .. dt .. float endTime]
        Scatter(
            x = xValues,
            y = wpValues,
            mode = "lines",
            name = "Wiener Process",
            line = Line(color = "grey", width = 2)
        )
    
    let layout =
        Layout(
            title = "Wiener Process",
            annotations = [
                Annotation(
                    text = sprintf "Start Time: %i,\nEnd Time: %i,\nΔt: %.3g" startTime endTime dt,
                    xref = "paper",
                    yref = "paper",
                    x = 0.5,
                    y = -0.15,
                    showarrow = false
                )
            ]
        )
    let chart = Chart.Plot(plotWienerProcess, layout)
    chart.Show()

// plotting a Geometric Brownian Motion
let plotGBM() =
    let startTime = 0
    let endTime = 10
    let dt = 0.01
    let currentPrice = 100.0
    let drift = 0.01
    let volatility = 0.05
    let wpValues = WienerProcess(startTime, endTime, dt)
    let gbmValues = GeometricBrownianMotion(currentPrice, startTime, endTime, dt, drift, volatility, wpValues)
    
    let plotGBM = 
        Scatter(
            x = [float startTime .. dt .. float endTime],
            y = gbmValues,
            mode = "lines",
            line = Line(color = "grey", width = 2),
            name = "Geometric Brownian Motion"
        )
    let layout =
        Layout(
            title = "Geometric Brownian Motion",
            annotations = [
                Annotation(
                    text = sprintf "S₀: %.3g,\nStart Time: %i,\nEnd Time: %i,\nΔt: %.3g,\nμ: %.3g,\nσ: %.3g" currentPrice startTime endTime dt drift volatility,
                    xref = "paper",
                    yref = "paper",
                    x = 0.5,
                    y = -0.15,
                    showarrow = false
                )
            ]
        )
    let chart = Chart.Plot(plotGBM, layout)
    chart.Show()


// plotting a European Call Option
let plotEuropeanCall() =
    let startTime = 0
    let endTime = 10
    let dt = 0.01
    let currentPrice = 100.0
    let drift = 0.01
    let volatility = 0.05
    let strike = 95.0
    let interestRate = 0.05
    
    let gbmValuesList =
        [ for i in 0..9 ->
            Random.SetSampleGenerator(Random.RandThreadSafe(i)) // new seed everytime so we get new GBM path
            let wpValues = WienerProcess(startTime, endTime, dt)
            let gbmValues = GeometricBrownianMotion(currentPrice, startTime, endTime, dt, drift, volatility, wpValues)
            gbmValues ]

    let callValuesList =
        [ for gbmValues in gbmValuesList -> // map I * payoff function to GBM
            let callValues = List.map (fun value -> I interestRate endTime * max (value - strike) 0.0) gbmValues
            callValues ]

    let averageCallValues =
        callValuesList
        |> List.reduce (List.map2 (+))
        |> List.map (fun sum -> sum / float (List.length callValuesList))

    let traces =
        [ for i in 0..9 ->
            let x = [ float startTime .. dt .. float endTime ]
            let y = callValuesList.[i]
            Scattergl(x = x, y = y, mode = "lines", line = Line(width = 1.0, color = "grey"), name = "Path " + string (i + 1)) ]
        |> List.append [ Scattergl(x = [ float startTime .. dt .. float endTime ], y = averageCallValues, mode = "lines", line = Line(width = 2.0, color = "red"), name = "Average") ]

    let layout =
        Layout(
            title = "European Call Option Valuation",
            annotations = [
                Annotation(
                    text = sprintf "S₀: %.3g,\nStart Time: %i,\nEnd Time: %i,\nΔt: %.3g,\nμ: %.3g,\nσ: %.3g,\nStrike: %.3g,\nInterest rate: %.3g%%" currentPrice startTime endTime dt drift volatility strike (interestRate * 100.0),
                    xref = "paper",
                    yref = "paper",
                    x = 0.5,
                    y = -0.15,
                    showarrow = false
                )
            ]
        )

    let chart = Chart.Plot(traces, layout)
    chart |> Chart.Show





(* A contract that includes the acquisition of a European Call 
Option with the underlying stock AAPL, a strike price of 100 
dollars and maturity in 30 days. *)
let EuropeanCallOption (i : float) : Contract =
    let underlying = "AAPL"
    let strike = 100.0 
    let maturity = 30
    let ccy = USD
    let payoff = 
        Max(Value 0.0,
            Sub(Underlying(underlying, maturity), 
                Value (strike * I (float maturity - i) maturity)))
    Acquire(float maturity - i, maturity, Scale(payoff, One ccy))

