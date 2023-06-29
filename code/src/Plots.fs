module Plots
open Simulations
open FSharp.Stats
open FSharp.Stats.Distributions
open XPlot.Plotly


(* This file is for replicating the plots in the report *)



// We have to define the GBM, GBMPath and discount function to take as input a float because then we can plot it elegantly.
// We still keep the original definition in Simulation.fs since we are working with days in the DSL.
let GBMPlot(initialPrice : float) (T : float) (mu : float) (sigma : float) : float =
    let Td = T/365.
    let normal = ContinuousDistribution.normal 0.0 1.0
    let drift = (mu - 0.5 * (sigma ** 2.0)) * Td
    let diffusion = sigma * (sqrt Td) * normal.Sample()
    initialPrice * exp(drift + diffusion)

let r = 0.02 // defined globally because we use this value in the plot
let IPlot(t) = 
    exp(-r/365. * t)


let GBMPathPlot (initialPrice: float) (T: float) (dt: float) (mu: float) (sigma: float) : float list =
    let normal = ContinuousDistribution.normal 0.0 1.0
    let Td = float T / 365.0
    let numSteps = int (float Td / dt)
    let drift = (mu - 0.5 * sigma**2.) * dt
    let diffusion = sigma * sqrt(dt)

    let simulateSteps (initialPrice: float) : float list =
        // Generate the Wiener process
        let wienerProcess = List.init numSteps (fun _ -> normal.Sample() * sqrt(dt))
        // Scan the Wiener process to compute the GBM
        let prices = List.scan (fun price dW -> price * exp(drift + diffusion * dW)) initialPrice wienerProcess
        prices

    simulateSteps initialPrice




// plotting a Wiener Process
let plotWienerProcess() =
    let endTime = 10
    let dt = 0.01
    let wpValues = WienerProcess endTime dt
    let plotWienerProcess =
        let xValues = [float 0 .. dt .. float endTime]
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
                    text = sprintf "\nEnd Time: %i,\nΔt: %.3g" endTime dt,
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

// Asian Option
let plotAsian() =
    let endTime = 365.
    let dt = 1./365.
    let initialPrice = 100.0
    let mu = 0.02
    let sigma = 0.2
    let sims = 9
    let dt_string : string = 
        match dt with
        | x when x = 1./365. -> "1/365" // for the report
        | _ -> sprintf "%.5g" dt
    let dates = List.map (fun x -> x * endTime) [0.0 .. dt .. 1.0]
    let gbmValues =
        [ for i in 0..sims ->
            Random.SetSampleGenerator(Random.RandThreadSafe(i)) // new seed everytime so we get new GBM path
            let gbmValues : float list = GBMPathPlot initialPrice endTime dt mu sigma
            gbmValues ]
    let asian =
        gbmValues
        |> List.reduce (List.map2 (+))
        |> List.map (fun sum -> sum / float (List.length gbmValues))



    let gbms =
        [ for i in 0..sims ->
            Scattergl(
                x = dates,
                y = List.item i gbmValues,
                mode = "lines",
                line = Line(width = 2.0, color = "grey"),  // Update color to grey
                name = sprintf "GBM %i" (i+1)
            ) ]
    let averageTrace =
        Scattergl(
            x = dates,
            y = asian,
            mode = "lines",
            line = Line(width = 2.0, color = "red"),  // Update color to red
            name = "Average"
        )

    let traces = gbms @ [averageTrace]
        


    let layout =
        Layout(
            title = "Asian Option Valuation",
            annotations = [
                Annotation(
                    text = sprintf "S₀: %.3g,\nStart Time: %i,\nEnd Time: %.3g %s,\n Δt: %s,\nμ: %.3g,\nσ: %.3g,\nInterest rate: %.3g%%" initialPrice 0 endTime "days" dt_string mu sigma (r * 100.0),
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

// plotting a European Call Option. This plot is not in the report.
let plotEuropeanCall() =
    let endTime = 10.
    let dt = 0.01
    let initialPrice = 100.0
    let mu = 0.3
    let sigma = 0.0001
    let strike = 100.0
    let interestRate = 0.02
    let sims = 9

    let dates : float list = [0.0 .. dt .. endTime]
    let gbmValuesList : float list list =
        [ for i in 0..sims ->
            Random.SetSampleGenerator(Random.RandThreadSafe(i)) // new seed everytime so we get new GBM path
            let gbmValues : float list = List.map(fun T -> GBMPlot initialPrice T mu sigma) dates
            gbmValues]

    let callValuesList =
        [ for gbmValues in gbmValuesList -> // map I * payoff function to GBM
            let callValues = List.map2 (fun value t -> IPlot(t) * max (value - strike) 0.0) gbmValues dates
            callValues ]

    let averageCallValues =
        callValuesList
        |> List.reduce (List.map2 (+))
        |> List.map (fun sum -> sum / float (List.length callValuesList))
    let traces =
        let simulationTraces =
            [ for i in 0..sims ->
                let callValues =
                    List.map2
                        (fun value date ->
                            (IPlot date) * max (value - strike) 0.0)
                            (List.item i gbmValuesList) dates
                Scattergl(x = [0.0 .. dt .. endTime ], y = callValues, mode = "lines", line = Line(width = 1.0, color = "grey"), name = sprintf "Path %i" i) ]
    
        let averageTrace = Scattergl(x = [ 0.0 .. dt .. endTime ], y = averageCallValues, mode = "lines", line = Line(width = 2.0, color = "red"), name = "Average")

        simulationTraces @ [averageTrace]

    let layout =
        Layout(
            title = "European Call Option Valuation",
            annotations = [
                Annotation(
                    text = sprintf "S₀: %.3g,\nStart Time: %i,\nEnd Time: %.1g %s,\nμ: %.3g,\nσ: %.3g,\nStrike: %.3g,\nInterest rate: %.3g%%" initialPrice 0 endTime "days" mu sigma strike (interestRate * 100.0),
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
    
    (*
let blackScholes() =
    let calculateBlackScholesCallPrice s0 x r t sigma =
        let d1 = (log(s0 / x) + (r + (sigma**2.0) / 2.0) * t) / (sigma * sqrt(t))
        let d2 = d1 - sigma * sqrt(t)
        let normal = ContinuousDistribution.normal 0.0 1.0 // Make sure to import the necessary library
        let nd1 = normal.CDF(d1)
        let nd2 = normal.CDF(d2)
        let callPrice = s0 * nd1 - x * exp(-r * t) * nd2
        callPrice


    let S = 100.0
    let T = float (maturity ec1) / 365. // 30
    let r = 0.02
    let sigma = 0.05
    let K = 95.

    let blackScholesPrice = calculateBlackScholesCallPrice S K r T sigma
    printfn "Black-Scholes Call Price: %f" blackScholesPrice
  *)