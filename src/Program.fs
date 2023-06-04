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
        "plotAsian", (fun () -> plotAsian())
        "plotEuropeanCall", (fun () -> plotEuropeanCall())
    ]

[<EntryPoint>]
let main argv =
    (*
    printfn "%A" sec1
    printfn "%A" (blackScholes())
    
    let gbm() : float = List.last (GBMPath 100.0 3650 0.01 0.02 0.002)
    printfn "%A" (List.init 10_000 (fun _ -> gbm()) |> List.average)

    *)

    let cc = Acquire(10, Scale(Underlying("foo", 0), One DKK))

    let ortest = flows cc
    printfn "%A" ortest



        (*
    let contractio =
        All[
            Acquire(2, Scale(Underlying("AAPL", 5), One DKK))
            ]
           // Acquire(10, Scale(Underlying("AAPL", 2), One USD))
          // Acquire(15, Or(Acquire(10,Scale(Value 100.0, One DKK)), Acquire(10, Scale(Value 100.0, One DKK))))
          
    let cflows = maturity contractio
    

    printfn "start %A" contractio
    printfn "after %A" cflows
    
    *)
    //let sim1 = simulateContract 10_000 k
    //let sim2 = simulateContract 10_000 ec1
    //printfn "%A" (sim1, sim2)

    //printfn "more simple %A" moresimple
    let seed = 1
    Random.SetSampleGenerator(Random.RandThreadSafe(seed))
    //printfn "black scholes %A" (blackScholes())
    //let sim = simulateContract 10_000 EuropeanCallOption
    //printfn "sim %A value" sim
    let random = new Random()
    let mutable seed = random.Next()
    let parseSeed argv = // check if the user has given a seed as the first argument or not
        match argv with
        | (arg: string) :: args when Int32.TryParse(arg, &seed) -> seed, args
        | args -> random.Next(), args
    let (seed, args) = parseSeed (List.ofArray argv) 
    //let cc : (string * int) List = underlyings (Acquire(0.02, 10, Scale(Underlying("AAPL", 10), One DKK))) 0
    //printfn "%A" (simulateContract 10_000 EuropeanCallOption)
    //printfn "black scholes %A" (blackScholes())
    //printfn "Using seed %i." seed
    Random.SetSampleGenerator(Random.RandThreadSafe(seed))

    //iterate through the arguments given by the user
    args
    |> List.iter (fun arg ->
        match plotsDict.TryGetValue arg with
        | (true, func) -> 
            printfn "Running function %s" arg
            func()
        | (false, _) -> printfn "%s is a invalid function name" arg)
    0