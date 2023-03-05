module Analysis
open FSharp.Stats
open FSharp.Math
open FSharp.Stats.Distributions

// Wiener Process
let wienerProcess (startTime : float) (endTime : float) (dt : float) (r : float) (sigma : float) : (float list * float list) =
    let normalDistribution = ContinuousDistribution.normal r (sigma**2.0)
    let numSteps = int ((endTime - startTime) / dt)
    let t = [startTime .. dt .. endTime]
    let sampleValues = List.init numSteps (fun _ -> normalDistribution.Sample() * sqrt(dt))
    let results = List.scan (+) 0.0 sampleValues
    (results, t)
    
// Geometric Brownian Motion
let GBM (startTime : float) (endTime : float) (dt : float) (r : float) (sigma : float) : float list =
    let (w, t) = wienerProcess startTime endTime dt r sigma
    List.mapi (fun i x -> x * exp((r - 0.5 * sigma**2.0) * t.[i] + sigma * x)) w
