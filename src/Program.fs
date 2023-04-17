module Program
open Domain
open Analysis
open Tests
open Simulations
open FSharp.Data
open FSharp.Data.CsvExtensions
open XMLFunctions // Import XMLFunctions module

let main args =
    //let callValue = simulateContract exampleEuropeanCallOption
    //let putValue = simulateContract exampleEuropeanPutOption
    //let parity = callValue - putValue - (130.0 - 100.0 * exp(-0.02*30.0))

    //let contractValue = simulateContract newContract
    //let dikuA = Underlying("DIKU A/S", 10)
    //let dikuB = Underlying("DIKU A/S", 5)
    //let contracts = All([Scale(dikuA, One USD); Scale(dikuB, One USD)])
    //let sim : float = simulateContract contracts
    //let stockObs (c : Contract List) : (Obs List) = List.concat (List.map getStocksAsObs c)
    //let result = stockObs [exampleEuropeanCallOption; exampleEuropeanPutOption; exampleForward]
    //let realresult = List.map getUnderlyingInfo result |> List.distinct
    //printfn "%A" callValue
    //printfn "%A" putValue
    //printfn "%s %A" "parity" parity
    //printfn "%A" (testParity (exampleEuropeanCallOption, exampleEuropeanPutOption, 100.0))
    printfn "%s %A" "result" (simulateContract exampleEuropeanCallOption)
    0.0
main []