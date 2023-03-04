module Analysis
open FSharp.Stats
open FSharp.Math
open FSharp.Stats.Distributions

let normalDistribution = ContinuousDistribution.normal 0.0 1.0
// Wiener Process
let wienerProcess (startTime : float) (endTime : float) (dt : float) : (float list * float list) = 
    let mutable currentTime = startTime
    let mutable results = [0.0] // Add zero as the initial value
    let mutable t = [startTime] // Add the start time as the initial time
    while currentTime <= endTime do
        let sample = normalDistribution.Sample()
        let value = sample * sqrt(dt)
        results <- results @ [value + List.last results] // Add the last value to the new value
        t <- t @ [currentTime]
        currentTime <- currentTime + dt
    (results, t)

// Geometric Brownian Motion
let GBM (startTime : float) (endTime : float) (dt : float) (r : float) (sigma : float) : float list =
    let (w, t) = wienerProcess startTime endTime dt 
    List.mapi (fun i x -> x * exp((r - 0.5 * sigma**2.0) * t.[i] + sigma * x)) w
// Asian Option Pricing 
let Asianoption (S : float list) =
    mean(S) 
