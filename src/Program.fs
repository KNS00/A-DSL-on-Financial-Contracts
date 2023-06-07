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

    let C_ (ccy : Currency) : float =
        1.0 // Assume same rate for each currency for simplification
    let I_ (t : int) : float = 1.0 // Assume no discount rate for simplification
    // new environment for testing
    let E_ (z : string * int) : float =
        match z with
        | ("AAPL", _) -> 150.0
        | ("MSFT", _) -> 10.0

    let fz (c : Contract) : float = Evaluations.evalc C_ I_ E_ c

    // Choose
    let ec_1 : Contract = europeanCall2 0 "AAPL" 100.0 USD // we have (150 - 100)^+ = 50 since S_T = 150 according to E_
    let ec_2 : Contract = europeanCall2 0 "MSFT" 100.0 USD // we have (10 - 100)^+ = 0 since S_T = 150 according to E_
    let cy1 = Scale(Value 10.0, One USD)
    let cy2 = Scale(Value 0.0, One USD)

    let ccz_ = Acquire(0, Or(Acquire(0, Or(cy1, cy2)),Acquire(0, Or(cy1, cy2))))

    let fz_ : Contract = choose fz (Acquire(0, (Or(ec_1, ec_2))))
    
    printfn "fz_ %A" fz_




    let ec1 = europeanCall2 0 "AAPL" 95. USD
    let sec1 = simulateContract 10_000 ec1

    let cc1 = flow 0 100.0 USD 
    let cc2 = flow 0 200.0 USD
    let cc3 = flow 0 300.0 USD
    let cc4 = flow 0 400.0 USD
    let ccc = Or(cc1, Or(cc2, Or(cc3, cc4)))

    let f (c : Contract) : float =
        simulateContract 10_000 c
    printfn "before %A" ccc
    printfn "after %A" (choose f ccc)


    let cz : Contract = Scale(Value 0.0, Scale(Value 10.0, Scale(Value 10.0, Scale(Value 10.0, Scale(Value 1.0, One DKK)))))
    let z = simplify dummyE cz
    printfn "before %A" cz
    printfn "after %A" z
    printfn "after after %A" (simplify dummyE z)


    let c1 = Acquire(1, One DKK)
    let c2 = Acquire(17, Scale(Underlying("AAPL", 0), One DKK))
    let c3 = Acquire(1, Scale(Underlying("AAPL", 0), One DKK))
    let c4 = Acquire(1, Scale(Underlying("MSFT", 0), One DKK))
    let c5 = All[c1; c2; c3; c4]
    let res = advance dummyE 1 c5



    (*
    (*
    printfn "%A" sec1
    printfn "%A" (blackScholes())
    
    let gbm() : float = List.last (GBMPath 100.0 3650 0.01 0.02 0.002)
    printfn "%A" (List.init 10_000 (fun _ -> gbm()) |> List.average)
    *)
    let c : Contract =
        All[
            Acquire(10, Scale(Underlying("AAPL", 2), One USD))
            Acquire(10, Scale(Underlying("AAPL", 0), One USD))
            ]

    let sc = causal c
    printfn "Before: \n %A" c
    printfn "After: \n %A" (sc)

    let seed = 1
    Random.SetSampleGenerator(Random.RandThreadSafe(seed))
    let gbmp() = GBMPath 100.0 365 0.01 0.02 0.05
    let gbmz() = GBM 100.0 365 0.02 0.05
   (* let gbmps = List.init 10_000 (fun _ -> List.last(gbmp()))
    let gbmzz = List.init 10_000 (fun _ -> gbmz())
    printfn "%A" (gbmps |> List.average)
    printfn "%A" (gbmzz |> List.average)
    *)
    let cc = Acquire(10, Scale(Underlying("foo", 0), One DKK))
    
    let ortest = flows cc
  //  printfn "%A" ortest



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
    let sim1 = simulateContract 10_000 ec1
    printfn "%A" sim1

    //printfn "more simple %A" moresimple
   // let seed = 1
   // Random.SetSampleGenerator(Random.RandThreadSafe(seed))
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
        *)
    0