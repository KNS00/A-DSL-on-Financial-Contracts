module Program
open Domain
open Analysis
open Evaluations
open Simulations
open FSharp.Data
open XMLFunctions
open Plots
open Examples
open Management
open FSharp.Stats
open FSharp.Stats.Distributions
open XPlot.Plotly
open System

let plotsDict = 
    dict [
        "plotWienerProcess", (fun () -> plotWienerProcess())
        "plotGBM", (fun () -> plotGBM())
        "plotEuropeanCall", (fun () -> plotEuropeanCall())
    ]

[<EntryPoint>]
let main argv =
    printfn "start %A" contractio
    printfn "after %A" cflows
    //printfn "more simple %A" moresimple
    //blackScholes()
    //let value = simulateContract 1_000 0.01 EuropeanCallOption
    //printfn "%s %A" "value of call DSL simulation" value

    let random = new Random()
    let mutable seed = random.Next()
    let parseSeed argv = // check if the user has given a seed as the first argument or not
        match argv with
        | (arg: string) :: args when Int32.TryParse(arg, &seed) -> seed, args
        | args -> random.Next(), args

    let (seed, args) = parseSeed (List.ofArray argv)
    printfn "Using seed %i." seed
    Random.SetSampleGenerator(Random.RandThreadSafe(seed))

    // iterate through the arguments given by the user
    args 
    |> List.iter (fun arg ->
        match plotsDict.TryGetValue arg with
        | (true, func) -> 
            printfn "Running function %s" arg
            func()
        | (false, _) -> printfn "%s is a invalid function name" arg)
    0