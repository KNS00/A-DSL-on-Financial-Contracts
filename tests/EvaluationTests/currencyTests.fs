module evalccyTests

open Domain
open Evaluations
open Simulations
open Analysis
open Tests
open Xunit

(*
// NUnit code
let testEvalccy (testCases: (Currency * float) list) =
    let tolerance = 1e-7
    testCases
    |> List.iter (fun (currency, expectedRate) ->
        evalccy currency |> should (equalWithin tolerance) expectedRate)

let sampleCurrencies : (Currency * float) list = [
    EUR, 1.10;
    GBP, 1.24;
    DKK, 0.15;
    USD, 1.0
]

[<TestFixture>]
module EvalccyTests =
    let ``Evalccy should evaluate currencies correctly``() =
        testEvalccy sampleCurrencies


        *)