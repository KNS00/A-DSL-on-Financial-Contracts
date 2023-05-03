module observableTests
open Domain
open Evaluations
open Simulations
open Analysis
open FsUnit
open Xunit

let testE (s: string, t: int) = 
    match s, t with
    | "AAPL", 1 -> 100.23
    | "AAPL", 3 -> 104.33
    | "AAPL", 5 -> 108.11
    | "GOOG", 1 -> 200.54
    | "GOOG", 5 -> 216.34
    | "MSFT", 2 -> 23.03
    | _ -> failwith "Stock not found"

let testEvalo(testCases : (Obs * float) list) : unit =
    testCases
    |> List.iter (fun (input, expectedOutput) ->
        evalo testE input |> should equal expectedOutput)

[<Fact>]
let ValueTestCases : (Obs * float) list = [
    (Value 10.0, 10.0);
    (Value 50.0, 50.0);
    (Value 0.0, 0.0);
    (Value -10.0, -10.0);
    (Value 956542.34534, 956542.34534);
    ]
let ``test Value constructor for evalo``() =
    testEvalo ValueTestCases

[<Fact>]
let UnderlyingTestCases : (Obs * float) list = [
    (Underlying("AAPL", 1),     testE("AAPL", 1));
    (Underlying("DIKU", 3),     testE("DIKU", 3));
    (Underlying("GOOG", -17),   testE("GOOG", -17));
    (Underlying("GOOG", 1),     testE("GOOG", 1));
    (Underlying("MSFT", 2),     testE("MSFT", 2));
    (Underlying("AAPL", 3),     testE("AAPL", 3));
    (Underlying("AAPL", 5),     testE("AAPL", 5));
    (Underlying("AAPL", 6),     testE("AAPL", 6));
]

let ``test Underlying constructor for eval``() =
    testEvalo UnderlyingTestCases


[<Fact>]
let MulTestCases = [
    (Mul(Value 0.0, Value 0.0), 0.0);
    (Mul(Value 1.0, Value 0.0), 0.0);
    (Mul(Value 0.0, Value 1.0), 0.0);
    (Mul(Value 0.5, Value 0.5), 0.25);
    (Mul(Value 3.0, Value 4.0), 12.0);
    (Mul(Value 10.0, Value -10.0), -100.0);
    (Mul(Value -10.0, Value 10.0), -100.0);
    (Mul(Value -5.0, Value -5.0), 25.0);
    (Mul(Value -1.0, Value -1.0), 1.0);
    (Mul(Value 1.5, Value 2.5), 3.75);
    (Mul(Value 2.0, Value 2.0), 4.0);
    (Mul(Value 0.0, Value 10.0), 0.0);
    (Mul(Value 10.0, Value 0.0), 0.0);
    (Mul(Value 2.5, Value 2.5), 6.25);
    ]

let ``test Mul constructor for evalo``() =
    testEvalo MulTestCases

[<Fact>]
let AddTestCases =
    [
    (Add(Value 10.0, Value 10.0), 20.0);
    (Add(Value 0.0, Value 0.0), 0.0);
    (Add(Value 1.0, Value 0.0), 1.0);
    (Add(Value 0.0, Value 1.0), 1.0);
    (Add(Value 0.5, Value 0.5), 1.0);
    (Add(Value 3.0, Value 4.0), 7.0);
    (Add(Value 10.0, Value -10.0), 0.0);
    (Add(Value -10.0, Value 10.0), 0.0);
    (Add(Value -5.0, Value -5.0), -10.0);
    (Add(Value -1.0, Value -1.0), -2.0);
    (Add(Value 1.5, Value 2.5), 4.0);
    (Add(Value 2.0, Value 2.0), 4.0);
    (Add(Value 0.0, Value 10.0), 10.0);
    ]

let ``test Add constructor for eval``() =
    testEvalo AddTestCases

[<Fact>]
let SubTestCases : (Obs * float) list = [
    Sub(Value(10.0), Value(5.0)), 5.0;
    Sub(Value(5.0), Value(10.0)), -5.0;
    Sub(Value(0.0), Value(0.0)), 0.0;
    Sub(Value(-5.0), Value(-5.0)), 0.0;
    Sub(Value(2.5), Value(1.0)), 1.5;
    Sub(Value(1.0), Value(2.5)), -1.5;
    Sub(Value(10.0), Value(-5.0)), 15.0;
    Sub(Value(-5.0), Value(10.0)), -15.0;
    Sub(Value(0.0), Value(10.0)), -10.0;
    Sub(Value(10.0), Value(0.0)), 10.0;
    ]

let ``test Sub constructor for eval``() =
    testEvalo SubTestCases

[<Fact>]
let MaxTestCases : (Obs * float) list = [
    Max(Value(10.0), Value(5.0)), 10.0;
    Max(Value(5.0), Value(10.0)), 10.0;
    Max(Value(0.0), Value(0.0)), 0.0;
    Max(Value(-5.0), Value(-5.0)), -5.0;
    Max(Value(2.5), Value(1.0)), 2.5;
    Max(Value(1.0), Value(2.5)), 2.5;
    Max(Value(10.0), Value(-5.0)), 10.0;
    Max(Value(-5.0), Value(10.0)), 10.0;
    Max(Value(0.0), Value(10.0)), 10.0;
    Max(Value(10.0), Value(0.0)), 10.0;
    ]

let ``test Max constructor for eval``() =
    testEvalo MaxTestCases

[<Fact>]
let CombinedTestCases : (Obs * float) list = [
    (Add(Value 10.0, Mul(Value 2.0, Value 5.0)), 20.0);
    (Mul(Add(Value 3.0, Value 4.0), Value 2.0), 14.0);
    (Sub(Max(Value 5.0, Value 10.0), Value 3.0), 7.0);
    (Mul(Value 5.0, Sub(Value 10.0, Value 2.0)), 40.0);
    (Max(Sub(Value 10.0, Value 5.0), Add(Value 3.0, Value 1.0)), 5.0);
    (Sub(Add(Value 3.0, Value 4.0), Mul(Value 2.0, Value 2.0)), 3.0);
    (Add(Mul(Value 2.0, Value 3.0), Sub(Value 10.0, Value 5.0)), 11.0);
    (Max(Sub(Value 1.0, Value 2.0), Mul(Value 3.0, Value 4.0)), 12.0);
]

let ``test combined constructors for evalo``() =
    testEvalo CombinedTestCases