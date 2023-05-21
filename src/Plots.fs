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
    let drift = 0.02
    let volatility = 0.03
    let strike = 100.0
    let interestRate = 0.02

    let dates = [float startTime .. dt .. float endTime]
    let gbmValuesList =
        [ for i in 0..100 ->
            Random.SetSampleGenerator(Random.RandThreadSafe(i)) // new seed everytime so we get new GBM path
            let wpValues = WienerProcess(startTime, endTime, dt)
            let gbmValues = GeometricBrownianMotion(currentPrice, startTime, endTime, dt, drift, volatility, wpValues)
            gbmValues ]

    let callValuesList =
        [ for gbmValues in gbmValuesList -> // map I * payoff function to GBM
            let callValues = List.map2 (fun value date -> exp(-interestRate * date/365.0) * max (value - strike) 0.0) gbmValues dates
            callValues ]

    let averageCallValues =
        callValuesList
        |> List.reduce (List.map2 (+))
        |> List.map (fun sum -> sum / float (List.length callValuesList))

    let traces =
        [ Scattergl(x = [ float startTime .. dt .. float endTime ], y = averageCallValues, mode = "lines", line = Line(width = 2.0, color = "red"), name = "Average") ]

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

let blackScholes() =
    let calculateBlackScholesCallPrice s0 x r t sigma =
        let tradingDaysPerYear = 252.
        let daily_r = exp(r / tradingDaysPerYear) - 1.0
        let daily_sigma = sigma / sqrt(tradingDaysPerYear)
        let t_days = t // already in days

        let d1 = (log(s0 / x) + (daily_r + (daily_sigma**2.0) / 2.0) * t_days) / (daily_sigma * sqrt(t_days))
        let d2 = d1 - daily_sigma * sqrt(t_days)
        let normal = ContinuousDistribution.normal 0.0 1.0
        let nd1 = normal.CDF(d1)
        let nd2 = normal.CDF(d2)
        let callPrice = s0 * nd1 - x * exp(-daily_r * t_days) * nd2
        callPrice

    let seed = 0
    let rnd = new Random(seed)

    let numSimulations = 1_000_000
    let dt = 0.01

    let T = float (getMaturityDate(EuropeanCallOption))
    let S = getPrice "AAPL" 0
    let r = 0.02
    let sigma = 0.005
    let K = 100.0
    printfn "Black scholes calculation %f" (calculateBlackScholesCallPrice S K r T sigma)
