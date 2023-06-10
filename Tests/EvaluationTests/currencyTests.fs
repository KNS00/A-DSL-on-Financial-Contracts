module currencyTests
open Domain
open Evaluations
open Simulations
open FsUnit
open Xunit

module currencyTests =
    let f (c : Currency) : float =
        match c with
        | USD -> 1.0
        | EUR -> 1.10
        | GBP -> 1.24
        | DKK -> 0.15
    let g (c : Currency) : float =
        match c with
        | USD -> 1.7
        | EUR -> 0.5
        | GBP -> 1.3
        | DKK -> 3.4
    let evalccyTestCases_f : List<obj[]> =
        [   [| EUR; f(EUR) |]
            [| GBP; f(GBP) |]
            [| DKK; f(DKK) |]
            [| USD; f(USD) |]
        ]
    let evalccyTestCases_g : List<obj[]> =
        [   [| EUR; g(EUR) |]
            [| GBP; g(GBP) |]
            [| DKK; g(DKK) |]
            [| USD; g(USD) |] ]
    [<Theory>]
    [<MemberData(nameof(evalccyTestCases_f))>]
    let ``evalccy should evaluate currencies correctly according to f``(ccy: Currency, expectedRate: float) =
        evalccy f ccy |> should equal expectedRate

    [<Theory>]
    [<MemberData(nameof(evalccyTestCases_g))>]
    let ``evalccy should evaluate currencies correctly according to g``(ccy: Currency, expectedRate: float) =
        evalccy g ccy |> should equal expectedRate
