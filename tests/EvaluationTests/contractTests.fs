module contractTests
open testFunctions
open Domain
open Evaluations
open Simulations
open Analysis
open FsUnit
open Xunit
open FSharp.Stats
open FSharp.Stats.Distributions
open XMLFunctions
module contractTests =
    let testE (s: string, t: int) = 
        match s, t with
        | "AAPL", 1 -> 100.23
        | "AAPL", 3 -> 104.33
        | "AAPL", 5 -> 108.11
        | "GOOG", 1 -> 200.54
        | "GOOG", 5 -> 216.34
        | "MSFT", 2 -> 23.03
        | _ -> 0.0

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


    // The One Constructor
    let OneTestCases : List<obj[]> = [   
        [| One EUR; evalccy EUR |]
        [| One DKK; evalccy DKK |]
        [| One GBP; evalccy GBP |]
        [| One USD; evalccy USD |]
    ]

    [<Theory>]
    [<MemberData(nameof(OneTestCases))>]
    let ``the evalc One constructor should return the currency evaluated in USD``(input : Contract, expectedValue : float) =
        evalc I testE input |> should (equalWithin 1e-7) expectedValue


    // The Scale constructor
    let ScaleTestCases : List<obj[]> = [
        [| Scale(Value 150.0, One EUR); 165.0 |] // 150.0 * 1 EUR = 165 USD
        [| Scale(Underlying("AAPL", 1), One EUR); 110.253 |] // 100.23 * 1 EUR = 110.253 USD
        [| Scale(Value 150.0, One GBP); 186.0 |] // 150.0 * 1 GBP = 186 USD
        [| Scale(Value 150.0, One DKK); 22.5 |] // 150 * 1 DKK = 22.5 USD
        [| Scale(Value 150.0, One USD); 150.0 |] // 150.0 * 1 USD = 150 USD
        [| Scale(Value -1.7, One EUR); -1.87 |] // -1.7 * 1 EUR = -1.87 USD
        [| Scale(Value 4.5, One GBP); 5.58 |] // 4.5 * 1 GBP = 5.58 USD
        [| Scale(Value 50000.0, One DKK); 7500.0 |] // 50000 * 1 DKK = 7500 USD
        [| Scale(Value -2.5, One USD); -2.5 |] // -2.5 * 1 USD = -2.5 USD
    ]

    // Executing tests for Scale
    [<Theory>]
    [<MemberData(nameof(ScaleTestCases))>]
    let ``the Scale constructor should scale a contract with an obs correctly`` (c : Contract, expectedValue : float) =
        evalc I testE c |> should (equalWithin 1e-7) expectedValue

    // Check that Acquire correctly discounts back in time according to the I function
    let AcquireTestCases : List<obj[]> = [
        // Acquire at time 0 should be equal to the current value of the contract
        [| Acquire(0, One EUR); evalc I testE (One EUR) |]

        // Acquire at a future time should be discounted by the I function
        [| Acquire(30, One EUR); I(30) * (evalc I testE (One EUR)) |]

        // Acquire at a past time should have a value of 0
        [| Acquire(-30, One DKK); I(-30) * (evalc I testE (One DKK)) |]

        // Acquire at a future time with a scale contract should be discounted by the I function
        [| Acquire(10, Scale(Value 500.0, One DKK)); I(10) * evalc I testE (Scale(Value 500.0, One DKK)) |]

        // Acquire at a future time with an underlying contract should be discounted by the I function
        [| Acquire(10, Scale(Underlying("AAPL", 5), One EUR)); I(10) * evalc I testE (Scale(Underlying("AAPL", 5), One EUR)) |]
    ]

    [<Theory>]
    [<MemberData(nameof(AcquireTestCases))>]
    let ``the Acquire constructor should correctly discount back in time according to the I function``(c : Contract, expectedValue : float) =
        evalc I testE c |> should (equalWithin 1e-7) expectedValue

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
    let ``evalc should evaluate a zcb correctly``(input : Contract, expectedValue: float) =
        evalc I testE input |> should (equalWithin 1e-7) expectedValue
    
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


    let dpbExpectedValue : float =
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


    // European Call Option example
    let exampleEuropeanCallOption: Contract =
        let underlying = "DIKU A/S"
        let strike = 100.0
        let maturity = 30
        let currency = USD
        let payoff =
            Max(
                Value 0.0,
                Sub(Underlying(underlying, maturity), Value strike)
            )
        Acquire(maturity, Scale(payoff, One currency))



    // Define the test case as an object list
    let callTestCase: List<obj[]> =
       // Calculate expected value of a European Call Option
        let callOptionExpectedValue (S: float) (X: float) (r: float) (tdays: float) (sigma: float) =
            let normal = ContinuousDistribution.normal 0.0 1.0
            let T = tdays / 365.0
            let d1 = (log(S / X) + (r + sigma ** 2. / 2.) * T) / (sigma * sqrt(T))
            let d2 = d1 - sigma * sqrt(T)
            S * normal.CDF(d1) - X * exp(-r * T) * normal.CDF(d2)
        
        let stockPrice : float = getPrice "DIKU A/S" 0
        let strikePrice = 100.0
        let riskFreeRate = 0.02
        let timeToExpirationDays = 30.0
        let mu = 0.01
        let volatility = 0.2
        //let value = I(getMaturityDate(exampleEuropeanCallOption)) * (max (stockPrice *exp(0.01 * 30.0 / 365.) - strikePrice) 0.0)
        let blackScholes = callOptionExpectedValue stockPrice strikePrice riskFreeRate (float(getMaturityDate(exampleEuropeanCallOption))) volatility
        [
            [| exampleEuropeanCallOption; blackScholes |]
        ]

    [<Theory>]
    [<MemberData(nameof(callTestCase))>]
    let ``evalc should evaluate a European Call Option correctly``(input: Contract) (expectedOutput: float) =
        let simulation = simulateContract input
//        printfn "%A" simulation
        //simulateContract input |> should (equalWithin 1e-2) expectedOutput
        simulation |> should (equalWithin 1e-2) expectedOutput









    
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
        
    
    





    

