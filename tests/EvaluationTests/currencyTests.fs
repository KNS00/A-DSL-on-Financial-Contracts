module evalccyTests

open Domain
open Evaluations
open Simulations
open Analysis
open Tests
open FsUnit
open Xunit

let testEvalccy(testCases : (Currency * float) list) : unit =
    testCases
    |> List.iter (fun (input, expectedOutput) ->
        evalccy input |> should equal expectedOutput)

[<Fact>]
let evalccyTestCases =[
    EUR, 1.10;
    GBP, 1.24;
    DKK, 0.15;
    USD, 1.10;
    ]
let ``evalccy should evaluate currencies correctly``() =
    testEvalccy evalccyTestCases
