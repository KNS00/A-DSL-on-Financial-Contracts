module Analysis
open FSharp.Stats

// initialize a normal distribution with mean 25 and standard deviation 0.1
let normalDistribution: Distributions.ContinuousDistribution<float,float> = Distributions.ContinuousDistribution.normal 25. 0.1

// draw independently 30 times from the given distribution 
let sample: float[]: float[] = Array.init 30 (fun _ -> normalDistribution.Sample())
