module currencyTests
open Domain
open Evaluations
open Simulations
open Analysis
open FsUnit
open Xunit

let evalccyTestCases : List<obj[]> =
    [   
        [| EUR; 1.10 |]
        [| GBP; 1.24 |]
        [| DKK; 0.15 |]
        [| USD; 1.0 |]
    ]

type TestCases =
    static member EvalccyTestCases = evalccyTestCases

[<Theory>]
[<MemberData(nameof(TestCases.EvalccyTestCases), MemberType = typeof<TestCases>)>]
let ``evalccy should evaluate currencies correctly``(ccy: Currency, expectedRate: float) =
    evalccy ccy |> should equal expectedRate
