module contractTests
open Domain
open Evaluations
open Simulations
open FsUnit
open FSharp.Stats
open Xunit
open Instruments
open Management
module contractTests =
    // Set a seed so that sampling is reproducible
    let seed = 1
    Random.SetSampleGenerator(Random.RandThreadSafe(seed))   
    

    let testE (s: string, t: int) = 
        match (s, t) with
        | ("AAPL", 0) -> 180.42
        | ("GOOG", 0) -> 129.44
        | ("MSFT", 0) -> 333.61
        | _ -> failwith "price not found"

    let cur (c : Currency) : float =
        match c with
        | USD -> 1.0
        | EUR -> 1.10
        | GBP -> 1.24
        | DKK -> 0.15
 
    // The One Constructor
    let OneTestCases : List<obj[]> = [   
        [| One EUR; evalccy cur EUR |]
        [| One DKK; evalccy cur DKK |]
        [| One GBP; evalccy cur GBP |]
        [| One USD; evalccy cur USD |]
    ]

    [<Theory>]
    [<MemberData(nameof(OneTestCases))>]
    let ``the evalc One constructor should return the currency evaluated in USD``(input : Contract, expectedValue : float) =
        evalc cur I testE input |> should (equalWithin 1e-7) expectedValue


    // The Scale constructor
    let ScaleTestCases : List<obj[]> = [
        [| Scale(Value 150.0, One EUR); 165.0 |] 
        [| Scale(Underlying("AAPL", 0), One EUR); 198.462 |] 
        [| Scale(Value 150.0, Scale(Value 150.0, One USD)); 22_500 |] // Logic principle 3 Scale(o1, Scale(o2.....
        [| Scale(Value 150.0, One DKK); 22.5 |] 
        [| Scale(Value 150.0, One USD); 150.0 |] 
        [| Scale(Value -1.7, One EUR); -1.87 |] 
        [| Scale(Value 4.5, One GBP); 5.58 |] 
        [| Scale(Value 50000.0, One DKK); 7500.0 |] 
        [| Scale(Value -2.5, One USD); -2.5 |] 
    ]

    // Executing tests for Scale
    [<Theory>]
    [<MemberData(nameof(ScaleTestCases))>]
    let ``the Scale constructor should scale a contract with an obs correctly`` (c : Contract, expectedValue : float) =
        evalc cur I testE c |> should (equalWithin 1e-7) expectedValue

    // check that Acquire discounts back in time according to the I function
    let AcquireTestCases : List<obj[]> =
        let fz : Contract = flow 10 100.0 DKK
        let fz2 : Contract = flow 0 100.0 DKK
        [
        [| Acquire(0, One EUR); 1.10 |] // Logic principle 4: Acquire(0, c) = c
        [| Acquire(30, One EUR); I 30 * 1.10 |]
        [| Acquire(10, Scale(Value 500.0, One DKK)); I 10 * 75.|]
        [| fz ; (I 10) * 15. |]
        [| Acquire(10, fz2); I 10 * 15.|]
        [| Acquire(10, Acquire(10, One USD)); I 20 * 1.0 |] // Logic principle 1: Acquire(t_1, Acquire(t_2....
        [| All[fz; fz2]; evalc cur I testE fz + evalc cur I testE fz2|]
    ]

    [<Theory>]
    [<MemberData(nameof(AcquireTestCases))>]
    let ``the Acquire constructor should correctly discount back in time according to the I function``(c : Contract, expectedValue : 'a) =
        evalc cur I testE c |> should (equalWithin 1e-7) expectedValue

    let GiveTestCases : List<obj[]> =
        let t1 = flow 10 100.0 USD
        let t2 = flow 100 100.0 USD
        let t3 = Acquire(0, Scale(Value 100.0, One USD))
        [
        [| Give(t1); -I(10) * 100.0  |]
        [| Give(t2); -I(100) * 100.0 |]
        [| Give(t3); -I(0) * 100.0 |]
        [| Acquire(0, Scale(Sub(Underlying("MSFT", 0), Underlying("AAPL", 0)), One USD)); 333.61 - 180.42 |]
        [| Give(Or(t1, t2)); -evalc cur I testE t1 |]
        [| Give(Give(One DKK)); 0.15 |] // Logic principle 2, Give(Give(c)) = c
        ]

    [<Theory>]
    [<MemberData(nameof(GiveTestCases))>]
    let ``the Give constructor should correctly reverse flows``(c : Contract, expectedValue : 'a) =
        evalc cur I testE c |> should (equalWithin 1e-7) expectedValue


    
    let OrTestCases : List<obj[]> =
        let o1 = flow 10 100.0 USD
        let o2 = flow 100 200.0 USD
        let o3 = flow 100 300.0 USD

        [
        [| Or(o1, o2); evalc cur I testE o2|]
        [| Or(o1, Or(o2, o3)); evalc cur I testE o3 |]
        ]
    [<Theory>]
    [<MemberData(nameof(OrTestCases))>]
    let ``the Or constructor should choose the maximum flow``(c : Contract, expectedValue : 'a) =
        evalc cur I testE c |> should (equalWithin 1e-7) expectedValue

    let simulateContractTest_NonStochastic : List<obj[]> =
        let o1 = flow 10 100.0 USD
        let o2 = flow 10 200.0 USD
        let o3 = Scale(Value 100.0, o1)
        [
        [|o1|]
        [|o2|]
        [|o3|]
        [|Or(o2, o3)|]
        [|Or(o1, Or(o2, o3))|]
        [|Acquire(10, Or(o1, o2))|]
        ]
    [<Theory>]
    [<MemberData(nameof(simulateContractTest_NonStochastic))>]
    let ``simulateContract should have the same output as evalc``(c : Contract) =
        simulateContract 1 c |> should (equalWithin 1e-7) (evalc cur I testE c)


    let dE_(s : string, t : int) : float = 100.0
    let lastPrincipleTestCases : List<obj[]> =
        [
        [| Scale(Value 0.0, One DKK); Scale(Value 100.0, All[]) |]
        [| Scale(Value 0.0, Acquire(10, Scale(Underlying("AAPL", 0), One DKK))); Scale(Value 700.0, All[]) |]
        ]

    [<Theory>]
    [<MemberData(nameof(lastPrincipleTestCases))>]
    let ``test last logic principle``(c1 : Contract, c2 : Contract) =
        evalc cur I dE_ c1 |> should equal (evalc cur I dE_ c2)

    let simulateContractStock : List<obj[]> =
        let c1 = Scale(Underlying("AAPL", 15), One USD)
        let c2 = Acquire(15, Scale(Underlying("AAPL", 0), One USD))
        let c3 = Acquire(365, Scale(Underlying("AAPL", 0), One USD))

        [
            [| c1; 100.0 * exp(0.02 * 15./365.) |]
            [| c2; I(maturity c1) * 100.0 * exp(0.02 * float (maturity c1)/365.) |]
            [| c2; I(maturity c1) * 100.0 * exp(0.02 * float (maturity c1)/365.) |]

        ]

    [<Theory>]
    [<MemberData(nameof(simulateContractStock))>]
    let ``simulateContract should simulate a stock price according to the expected value of the GBM`` (c : Contract) (q : float) =
        simulateContract 100_000 c |> should (equalWithin 1e-2) q

    let putcallParity : List<obj[]> =
        let T = 100
        let stock = "AAPL"
        let strike = 95.
        let ccy = USD
        let price : float = XMLFunctions.getPrice "AAPL" 0
        [
        [| europeanCall2 T stock strike ccy; europeanPut T stock strike ccy; strike; price |]
        ]
    [<Theory>]
    [<MemberData(nameof(putcallParity))>]
    let ``simulateContract should be able to validate the put call parity``(call : Contract, put : Contract, strike : float, s0 : float) =
        let call_p : float = simulateContract 100_000 call
        let put_p : float = simulateContract 100_000 put
        let parity : float = call_p + strike * I (maturity(call)) - (put_p + s0)

        parity |> should (equalWithin 1e-2) 0.0
