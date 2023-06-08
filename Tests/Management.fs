module ManagementTests
open Domain
open Management
open Examples
open FsUnit
open Xunit

module ManagementTests =
    let rec equalLists list1 list2 : bool =
        match list1, list2 with
        | [], [] -> true
        | [], _ | _, [] -> false
        | (x1, f1)::rest1, (x2, f2)::rest2 ->
            if x1 = x2 && f1 = f2 then
                equalLists rest1 rest2
            else
                false

    // Maturity
    let maturityTestCases : List<obj[]> =
        [   [| Acquire(10, Scale(Underlying("AAPL", 2), One USD)); 10 |]
            [| europeanCall2 17 "DIKU" 100.0 DKK ; 17 |]
            [| ccSwap 10 DKK USD 0.7 0.3 0.2 0.1 0.5; 10 |]
            [| Give(Acquire(10, Acquire(10, One DKK))); 20 |]
            [| Or(Acquire(10, One DKK), Acquire(20, One DKK)); 20 |]
        ]
    [<Theory>]
    [<MemberData(nameof(maturityTestCases))>]
    let ``Assert maturity date to contract``(c: Contract, expected : int) =
        maturity c |> should equal expected

    // Flows
    let flowsTestCases : List<obj[]> =
        let c1 = Acquire(0, One DKK)
        let c2 = Acquire(10, Scale(Underlying("MSFT", 0), One DKK)) 
        let c3 = Give(Acquire(37, One DKK)) 
        let c4 = Acquire(35, Or(c2, c3)) 

        [
          [| c1; [0, Certain(1.0, DKK)]|]
          [| c2; [(10, Uncertain)] |]
          [| All[c2; c3]; [(10, Uncertain); (37, Certain (-1.0, DKK))] |]
          [| c4; [(35, Choose ([(45, Uncertain)], [(72, Certain (-1.0, DKK))]))]|]
        ]
    [<Theory>]
    [<MemberData(nameof(flowsTestCases))>]
    let ``Assert flows``(c: Contract, expected : (int * flow) List ) =
        let z = equalLists (flows c) expected
        z |> should equal true

    // Causal with empty lists. 
    let causalTestCases_1 : List<obj[]> =
        let c1 = Acquire(0, One DKK)
        let c2 = Acquire(10, Scale(Underlying("MSFT", 0), One DKK)) 
        let c3 = Give(Acquire(37, One DKK)) 
        let c4 = Acquire(10, Scale(Underlying("AAPL", 5), One DKK ))
        [
          [| c1; [] |]
          [| c2; [] |]
          [| c3; [] |]
          [| All[c1; c2; c3]; [] |]
        ]
    [<Theory>]
    [<MemberData(nameof(causalTestCases_1))>]
    let ``Assert causal empty``(c: Contract, expected : 'a) =
        expected |> should equal []

    // Causal with non-empty lists.
    let causalTestCases_2 : List<obj[]> =
        let c1 = Acquire(10, Scale(Underlying("AAPL", 5), One DKK ))
        let c2 = Acquire(17, Scale(Underlying("AAPL", 17), One USD ))
        let c3 = Give(Scale(Value 100.0, Acquire(10, Scale(Underlying("AAPL", 5), One DKK ))))

        [
          [| c1; [(10, Causal(15))] |]
          [| c2; [(17, Causal(34))] |]
          [| c3; [10, Causal(15)] |]
          [| All[c1; c2; c3]; [(10, Causal 15); (17, Causal 34); (10, Causal 15)] |]
        ]
    [<Theory>]
    [<MemberData(nameof(causalTestCases_2))>]
    let ``Assert causal non-empty``(c: Contract, expected : 'a) =
        expected |> should equal (causal c)

    // Underlyings 
    let underlyingsTestCases : List<obj[]> =
        let u1 : Obs = Underlying("AAPL", 2)
        let u2 : Obs = Underlying("MSFT", 17)
        let u3 : Obs = Underlying("DB", 0)
        let u4 : Obs = Underlying("GOOG", 0)
        let c1 = Scale(u1, One DKK)
        let c2 = Scale(u2, One USD)
        let c3 = Scale(u3, Give(Acquire(10, One GBP)))
        let c4 = Scale(u4, Give(Acquire(15, One DKK)))
        let c5 =
            All[Scale(u1, One DKK); Scale(u1, One USD); Scale(u2, Acquire(10, One DKK));
                Scale(u3, Give(One DKK)); Scale(u1, Scale(u1, Scale(u1, Scale(u4, One DKK))))]
        [
        [| c1; ["AAPL", 2] |]
        [| c2; ["MSFT", 17] |]
        [| c3; ["DB", 0] |]
        [| c4; ["GOOG", 0] |]
        [| c5; [("AAPL", 2); ("MSFT", 17); ("DB", 0); ("GOOG", 0)] |]
        ]
    [<Theory>]
    [<MemberData(nameof(underlyingsTestCases))>]
    let ``Assert underlyings``(c : Contract, expected) =
        expected |> should equal (underlyings c)


    // Using this environment for testing
    let dE (k : (string * int)) : float =
        match k with
        | "AAPL", 0 -> 100.0
        | "AAPL", -1 -> 150.0
        | "MSFT", 0 -> 110.0
        | "MSFT", -1 -> 120.0
        | _ -> failwith "price not found"

    // Simplify
    let simplifyTestCases : List<obj[]> =
        let c1 = Acquire(0, Scale(Value 10.0, Scale(Value 2.0, Scale(Value 3.0, Scale(Value 4.0, Scale(Value 5.0, One DKK))))))
        let c2 = Acquire(0, Scale(Underlying("AAPL", 0), One DKK))
        let c3 = Acquire(10, Scale(Underlying("AAPL", 0), One DKK))
        let c4 = Give(Acquire(0, Scale(Value 100.0, One DKK)))
        let c5 = Give(Give(One DKK))
        let c6 = Or(c1, c2)
        let c7 = All[c3; c4; c5]
        let c8 = Scale(Value 100.0, All[])
        let c9 = Scale(Value 0.0, c1)
        let c10 = Acquire(10, Acquire(20, Acquire(30, One DKK)))
        [
        [|c1; Scale(Value 1200.0, One DKK)|]
        [|c2; Scale(Value 100.0, One DKK)|]
        [|c3; c3|]
        [|c4; Give(Scale(Value 100.0, One DKK))|]
        [|c5; One DKK|]
        [|c6; Or(Scale(Value 1200.0, One DKK), Scale(Value 100.0, One DKK) )|]
        [|c7; All[c3; Give(Scale(Value 100.0, One DKK)); One DKK] |]
        [|c8; All[] |]
        [|c9; Scale(Value 0.0, One DKK) |]
        [|simplify dE c9 ; All[] |] // double simplify to finish c9
        [|c10; Acquire(60, One DKK)|]
        ]
    [<Theory>]
    [<MemberData(nameof(simplifyTestCases))>]
    let ``Assert simplify``(c : Contract, expected) =
        expected |> should equal (simplify dE c)

    // Advance
    let advanceTestCases : List<obj[]> =
        let c1 = Acquire(1, One DKK)
        let c2 = Acquire(17, Scale(Underlying("AAPL", 0), One DKK))
        let c3 = Acquire(1, Scale(Underlying("AAPL", 0), One DKK))
        let c4 = Acquire(1, Scale(Underlying("MSFT", 0), One DKK))
        let c5 = All[c1; c2; c3; c4]
        let c6 = Give(c3)

        [
        [| c1; One DKK |]
        [| c2; Acquire(16, Scale(Underlying("AAPL", 0), One DKK)) |]
        [| c3; Scale(Value 100.0, One DKK)|]
        [| c4; Scale(Value 110.0, One DKK)|]
        [| c5; All[One DKK; Acquire(16, Scale(Underlying("AAPL", 0), One DKK)); Scale(Value 100.0, One DKK); Scale(Value 110.0, One DKK) ]|]
        [| c6; Give(Scale(Value 100.0, One DKK)) |]
        ]

    [<Theory>]
    [<MemberData(nameof(advanceTestCases))>]
    let ``Assert advance``(c : Contract, expected) =
        expected |> should equal (advance dE 1 c)


    
    let C_ (ccy : Currency) : float =
        1.0 // Assume same rate for each currency for simplification
    let I_ (t : int) : float = 1.0 // Assume no discount rate for simplification
    // new environment for testing
    let E_ (z : string * int) : float =
        match z with
        | ("AAPL", _) -> 150.0
        | ("MSFT", _) -> 10.0
        | _ -> failwith "price not found"

    let f (c : Contract) : float = Evaluations.evalc C_ I_ E_ c

    // Choose
    let chooseTestCases : List<obj[]> =
        let ec_1 : Contract = europeanCall2 0 "AAPL" 100.0 USD // we have (150 - 100)^+ = 50 since S_T = 150 according to E_
        let ec_2 : Contract = europeanCall2 0 "MSFT" 100.0 USD // we have (10 - 100)^+ = 0 since S_T = 150 according to E_

        let ex1 = flow 10 100.0 USD
        let ex2 = flow 15 150.0 USD
        let ex3 = flow 0 100.0 USD

        let c1 = Acquire(0, Or(ec_1, ec_2))

        let c2 = Or(ex1, Or(ex2, ex3))

        [
        [| ec_1; Acquire (0, Scale (Sub (Underlying ("AAPL", 0), Value 100.0), One USD))|]
        [| ec_2; Acquire(0, Scale(Value 0.0, One USD)) |]
        // we choose ec_1 due to the fact that AAPL > MSFT in price. Can be simplfiied more using the simplify function, we test later
        [| c1; Acquire(0, Acquire (0, Scale (Sub (Underlying ("AAPL", 0), Value 100.0), One USD))) |]
        [| c2; ex2 |]
        ]

    [<Theory>]
    [<MemberData(nameof(chooseTestCases))>]
    let ``Assert choose``(c : Contract, expected) =
        expected |> should equal (choose f c)

