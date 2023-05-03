module currencyTests
open Domain
open Evaluations
open Simulations
open Analysis
open FsUnit
open Xunit

let testEvalccy(testCases : (Currency * float) list) : unit =
    testCases
    |> List.iter (fun (input, expectedOutput) ->
        evalccy input |> should equal expectedOutput)

type CurrencyRate = {
    Currency: Currency
    Rate: float
}

let evalccyTestCases : seq<(Currency * float)> =
    seq {
        (EUR, 1.10)
        (GBP, 1.24)
        (DKK, 0.15)
        (USD, 1.10)
    }

type TestCases =
    static member EvalccyTestCases = evalccyTestCases



[<Theory>]
[<MemberData(nameof(evalccyTestCases), MemberType = typeof<TestCases>)>]
let ``evalccy should evaluate currencies correctly``(ccy: Currency, expectedRate: float) =
    evalccy ccy |> should equal expectedRate



    (*let evalccyTestCases : (Currency * float) list =[
    EUR, 1.10;
    GBP, 1.24;
    DKK, 0.15;
    USD, 1.10;
    ]
    *)
