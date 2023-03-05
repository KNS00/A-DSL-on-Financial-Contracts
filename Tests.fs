module Tests
open DSL
open FSharp.Stats
open FSharp.Stats.Distributions
open XPlot.Plotly
// Evaluate observables
let rec eval (o : Obs) = 
    match o with
    | Value n -> n
    | Underlying (s, t) -> 100.0 // 100 for now, need to fix this. It needs to return the price of stock s at time t.
    | Strike n -> n
    | Mul (c1, c2) ->
        let n1 = eval c1
        let n2 = eval c2
        n1 * n2
    | Add (c1, c2) ->
        let n1 = eval c1
        let n2 = eval c2
        n1 + n2
    | Sub (c1, c2) ->
        let n1 = eval c1
        let n2 = eval c2
        n1 - n2
    | Max (c1, c2) ->
        let n1 = eval c1
        let n2 = eval c2
        max (n1) (n2)


// Simulate stochastic proccesses.
let rec simulate (s : StochasticProcess) =
    match s with
    | WienerProcess (startTime, endTime, dt, r, sigma) ->
        let normalDistribution = ContinuousDistribution.normal r (sigma**2.0)
        let numSteps = int ((endTime - startTime) / dt)
        let sampleValues = List.init numSteps (fun _ -> normalDistribution.Sample() * sqrt(dt))
        let results = List.scan (+) 0.0 sampleValues
        results // return the wiener process value as a list
    | GeometricBrownianMotion(startTime, endTime, dt, r, sigma) ->
        let wp = WienerProcess (startTime, endTime, dt, r, sigma)
        let wpValues = simulate wp // generate the wiener process values once
        let numSteps = int ((endTime - startTime) / dt)
        let t = [startTime .. dt .. endTime]
        let sampleValues = 
            List.zip wpValues t |> List.map (fun (w,t) -> 
                // use the wiener process values and time values to calculate the GBM values
                let drift = (r - 0.5 * sigma**2.0) * t
                let diffusion = sigma * w
                exp(drift + diffusion)
            )
        sampleValues // return the GBM values as a list



(*
let isWienerProcess (s: StochasticProcess) =
    match s with
    | WienerProcess _ -> true
    | _ -> false
let isGBMProcess (s: StochasticProcess) =
    match s with
    | GeometricBrownianMotion _ -> true
    | _ -> false
*)

// getProcessInfo was made to get time intervals and type of stochastic process
let getProcessInfo (s : StochasticProcess) =
    match s with
    | WienerProcess (startTime, endTime, dt, _, _) -> startTime, endTime, dt,"Wiener Process"
    | GeometricBrownianMotion (startTime, endTime, dt, _, _) -> startTime,endTime,dt,"Geometric Brownian Motion"
// this function plots a stochastic process
let plotProcess (processes : StochasticProcess list) : unit =
  let startTime,endTime,_dt,name = getProcessInfo (List.head processes)
  let t = [startTime .. _dt .. endTime]
  let paths = List.map simulate processes
  let traces = paths |> List.mapi (fun i p ->
                  let name = sprintf "%s %d" name i
                  let trace = Scatter(x = t,y = p,name = name)
                  trace)
  let title = match processes with
              | [WienerProcess (_, _, _, _, _)] -> "Wiener Process Simulation"
              | [GeometricBrownianMotion (_, _, _, _, _)] -> "Geometric Brownian Motion Simulation"
              | _ -> "Mixed or Unknown Process Simulation"
                 // if List.forall isWienerProcess processes then "Wiener Process Simulation"
                 // elif List.forall isGBMProcess processes then "Geometric Brownian Motion Simulation"
                 // else "Mixed Process Simulation"
  Chart.Plot(traces) |> Chart.WithTitle(title) |> Chart.Show


// Test: Evaluation of observables
let obs1 = Add(Value 2.0, Mul(Value 3.0, Value 4.0))
let result1 = eval obs1
// Expected: 14 (2 + (3 * 4) = 14)

// Test: Evaluation of observables
let obs2 = Max(Value 5.0, Sub(Value 10.0, Mul(Value 2.0, Value 3.0)))
let result2 = eval obs2
// Expected: 1 (5 is greater than 10 - (2 * 3) = 4)


// Test: plot stochastic processes
let wp1 = WienerProcess (0.0, 10.0, 0.01, 0.0, 1.0)
let gbm1 = GeometricBrownianMotion (0.0, 10.0, 0.01, 0.0, 1.0)