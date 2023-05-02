module EvaluationTests
open Domain
open Evaluations
open Simulations
open Analysis
open Tests
open Xunit
open FsUnit

let testE (s: string, t: int) = 
    match s, t with
    | "AAPL", 1 -> 100.23
    | "AAPL", 3 -> 104.33
    | "AAPL", 5 -> 108.11
    | "GOOG", 1 -> 200.54
    | "GOOG", 5 -> 216.34
    | "MSFT", 2 -> 23.03
    | _ -> failwith "Stock not found"

module evaloTests =
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
    
module evalccyTests =
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


module evalcTests =
    let E(s,t) = 0.0
    let I (t : int) : float = 
        let yearlyInterestRate : float = 0.02 // assume 0.02
        let dailyInterestRate : float = yearlyInterestRate / 365.0
        let presentValue = 1.0/((1.0+dailyInterestRate)**float t)
        presentValue

    let flow(i: int, v: double, c: Currency) : Contract
        = Acquire(i,Scale(Value v, One c))


    [<Theory>]
    [<InlineData("EUR")>]
    [<InlineData("GBP")>]    
    [<InlineData("DKK")>]
    [<InlineData("USD")>]    
    let ``One c should equal the output of evalccy c``(ccyInput) =
        let ccy : Currency = stringToCurrency ccyInput
        let expectedOutput : float =  evalccy ccy
        let contract = One ccy 
        let output : float = evalc I E contract
        let tolerance = 1e-7
        let isEqual = abs (output - expectedOutput) <= tolerance
        isEqual |> should equal true


    let ScaleConstructorTests = [
        (Value 150.0,           One EUR, 165.0)     // 150.0 * 1 EUR = 165 USD
        (Underlying("AAPL", 1), One EUR, 165.0)     // 100.23 * 1 EUR = 165 USD
        (Value 150.0,           One GBP, 186.0)     // 150.0 * 1 EUR = 186 USD
        (Value 150.0,           One DKK, 22.5)      // 150 * 1 DKK = 22.5 USD
        (Value 150.0,           One USD, 150.0)     // 150.0 * 1 USD = 150 USD
        (Value -1.7,            One EUR, -1.87)     // -1.7 * 1 EUR = -1.87 USD
        (Value 4.5,             One GBP, 5.58)      // 4.5 * 1 GBP = 5.58 USD
        (Value 50000.0,         One DKK, 7500.0)    // 50000 * 1 DKK = 7500 USD
        (Value -2.5,            One USD, -2.5)      // -2.5 * 1 USD = -2.5 USD
        ]
    [<Fact>]
    let ``evalc with Scale should return the product of evalo and evalc``() =
        ScaleConstructorTests
        |> List.iter (fun (obs, ccy, expectedOutput) ->
            let contract: Contract = Scale (obs, ccy)
            let output : float = evalc I E contract
            let tolerance : float = 1e-7
            let isEqual : bool = abs (output - expectedOutput) <= tolerance
            isEqual |> should equal true)