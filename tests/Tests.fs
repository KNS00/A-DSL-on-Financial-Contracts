module Tests
open Domain
open Evaluations
open Simulations
open Analysis
let flow(i: int, v: double, c: Currency) : Contract
    = Acquire(i,Scale(Value v, One c))

let zcb1: Contract = 
  let maturityDate : int = 10
  flow(maturityDate, 100.0, USD)


let dividendPayingBond : Contract = 
  let maturityDate = 120
  let pct = 0.02
  let qPct : float = exp(pct * 1.0/4.0)
  let investment = 100.0
  let quarterlyPayments: Contract = 
    let payment = qPct * investment
    All([
        flow(30, payment, USD);
        flow(60, payment, USD);    
        flow(90, payment, USD);    
        flow(120, payment, USD)])
  Then(quarterlyPayments, flow(maturityDate, investment, USD))



// A contract containing the acquisition of 10 DKK, 5 days from now.
//let contract4 = flow(365, 100.0, EUR) // A contract containing the acquisition of 100 EUR 365 days from now.
//let contract5 = All([contract1; contract2; contract3; contract4])

// Call option
let equity = "DIKU A/S"
let maturity = 5
let strike = 50.0
let nominal = 1.0
let obs = 
    Max(Value 0.0, 
        Sub(
            Underlying(equity, maturity),
             (Value(strike)))) // max(0, E[S_T] - discount(strike))
let call_option : Contract = 
    Scale(
        Value nominal, 
        Acquire(maturity, Scale(obs, One USD))) // 1.0 * Acquire(5, 115 EUR)


let exampleEuropeanCallOption : Contract =
    let underlying = "DIKU A/S"
    let strike = 100.0
    let maturity = 30
    let currency = USD
    let payoff = 
        Max(Value 0.0, Sub(Underlying(underlying, maturity), Value strike))
    Scale(payoff, One currency)

let exampleEuropeanPutOption : Contract =
    let underlying = "DIKU A/S"
    let strike = 100.0
    let maturity = 30
    let currency = USD
    let payoff = 
        Max(Value 0.0, Sub(Value strike, Underlying(underlying, maturity)))
    Scale(payoff, One currency)

let exampleForward : Contract =
    let underlying = "DIKU A/S"
    let strike = 100.0
    let maturity = 30
    let currency = USD
    let discountedStrike = strike * I maturity
    let payoff = Sub(Underlying(underlying, maturity), Value discountedStrike)
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