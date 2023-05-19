module Program
open Domain
open Analysis
open Evaluations
open Simulations
open FSharp.Data
open XMLFunctions
open Plots
open FSharp.Stats
open XPlot.Plotly
open System
open System.IO
open System.CommandLine
open System.CommandLine.Invocation

let plotsDict = 
    dict [
        "plotWienerProcess", (fun () -> plotWienerProcess())
        "plotGBM", (fun () -> plotGBM())
        "plotEuropeanCall", (fun () -> plotEuropeanCall())
    ]

[<EntryPoint>]
let main argv =
    let random = new Random()
    let mutable seed = random.Next()
    let parseSeed argv = // check if the user has given a seed as the first argument or not
        match argv with
        | (arg: string) :: args when Int32.TryParse(arg, &seed) -> seed, args
        | args -> random.Next(), args

    let (seed, args) = parseSeed (List.ofArray argv)
    printfn "Using seed %i." seed
    Random.SetSampleGenerator(Random.RandThreadSafe(seed)) 

    args // iterate through the arguments given by the user
    |> List.iter (fun arg ->
        match plotsDict.TryGetValue arg with
        | (true, func) -> 
            printfn "Running function %s" arg
            func()
        | (false, _) -> printfn "%s is a invalid function name" arg)
    0