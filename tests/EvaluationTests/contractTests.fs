module contractTests
open Domain
open Evaluations
open Simulations
open Analysis
open FsUnit
open Xunit

module contractTests =
    let testE (s: string, t: int) = 
        match s, t with
        | "AAPL", 1 -> 100.23
        | "AAPL", 3 -> 104.33
        | "AAPL", 5 -> 108.11
        | "GOOG", 1 -> 200.54
        | "GOOG", 5 -> 216.34
        | "MSFT", 2 -> 23.03
        | _ -> failwith "Stock not found"

    [<Theory>] 
    [<InlineData(5, 0.9997260724)>]    // 1/((1+0,02/365))^5)    = 0.9997260724
    [<InlineData(20, 0.9989047398)>]   // 1/((1+0,02/365))^20)   = 0.9989047398
    [<InlineData(30, 0.9983575597)>]   // 1/((1+0,02/365))^30)   = 0.9983575597
    [<InlineData(365, 0.9801992104)>]  // 1/((1+0,02/365))^365)  = 0.9801992104
    [<InlineData(1000, 0.9466810723)>] // 1/((1+0,02/365))^1000) = 0.9466810723
    let ``discount function should correctly discount back the value``(input : int, expectedOutput) =
        let tolerance = 1e-7
        let output = I input
        let isEqual = abs (output - expectedOutput) <= tolerance
        isEqual |> should equal true

    // Introducing flow for shorthand notation
    let flow(i: int, v: double, c: Currency) : Contract
        = Acquire(i,Scale(Value v, One c))

    (* Acquire a bond that pays 100 USD in 10 days *)    
    let zcb: Contract = 
      let maturityDate : int = 10
      flow(maturityDate, 100.0, USD)

    // The expected value is the payment discounted by the maturity date
    let zcbExpectedValue = 100.0 * I(getMaturityDate(zcb))

    // Formuala the test case as a object list
    let zcbTestCase : List<obj[]> = [   
        [| zcb; zcbExpectedValue |]
    ]

    // Executing testing for the ZCB
    [<Theory>]
    [<MemberData(nameof(zcbTestCase))>]
    let ``evalc should evaluate a zcb correctly``(c : Contract, expectedValue: float) =
        evalc I testE c |> should (equalWithin 1e-7) expectedValue
    
    (* Acquire a bond that pays 100 USD in 120 days and pays 2 percent interest quarterly *)
    let dividendPayingBond : Contract = 
      let maturityDate = 120
      let pct = 0.02
      let investment = 100.0
      let payments: Contract = 
        let payment = 0.02 * investment
        All([
            flow(30, payment, USD);
            flow(60, payment, USD);    
            flow(90, payment, USD);    
            flow(120, payment, USD);
            flow(120, investment, USD)
            ])
      payments


    let dpbExpectedValue =
        let investment = 100.0
        let payment = 0.02 * investment
        payment * I(30)
        + payment * I(60)
        + payment * I(90)
        + payment * I(120)
        + investment * I(120)

    // Formuala the test case as a object list
    let dbpTestCase : List<obj[]> = [   
        [| dividendPayingBond; dpbExpectedValue |]
    ]

    // Executing testing for the DBP
    [<Theory>]
    [<MemberData(nameof(dbpTestCase))>]
    let ``evalc should evaluate a dbp correctly``(c : Contract, expectedValue: float) =
        evalc I testE c |> should (equalWithin 1e-7) expectedValue



    
    // Call option
    let exampleEuropeanCallOption : Contract =
        let underlying = "DIKU A/S"
        let strike = 100.0 
        let maturity = 30
        let currency = USD
        let payoff = 
            Max(Value 0.0,
                Sub(Underlying(underlying, maturity), Value (strike * I maturity)))
        Scale(payoff, One currency)
    
    let exampleEuropeanPutOption : Contract =
        let underlying = "DIKU A/S"
        let strike = 100.0 
        let maturity = 30
        let currency = USD
        let payoff = 
            Max(Value 0.0, Sub(Value (strike * I maturity), Underlying(underlying, maturity)))
        Scale(payoff, One currency)
    
    let exampleForward : Contract =
        let underlying = "DIKU A/S"
        let strike = 100.0 
        let maturity = 30
        let currency = USD
        let payoff = Sub(Underlying(underlying, maturity), Value (strike * I maturity))
        Scale(payoff, One currency)
    
    
    let exampleCurrencySwap : Contract =
        let notional1 = 1000000.0
        let currency1 = USD
        let notional2 = 900000.0
        let currency2 = EUR
        let maturity = 30
    
        let leg1 = Scale(Value notional1, One currency1)
        let leg2 = Scale(Value notional2, One currency2)
        Then(leg1, Acquire(maturity, leg2))
    
    let exampleFixedRateBond : Contract =
        let principal = 1000.0
        let rate = 0.05
        let maturity = 30
        let currency = USD
        let coupon = Scale(Value (principal * rate), One currency)
        let principalAtMaturity = Scale(Value principal, One currency)
        All [coupon; Acquire(maturity, principalAtMaturity)]
        
    
    





    

