module Examples
open Domain
open Evaluations
open Simulations
open Analysis


let i = 0.02

let flow(i : float, t: int, v: double, c: Currency) : Contract
    = Acquire(i, t, Scale(Value v, One c))

let zcb1: Contract = 
  let maturityDate : int = 10
  flow(i, maturityDate, 100.0, USD)


let dividendPayingBond : Contract = 
  let maturityDate = 120
  let pct = 0.02
  let qPct : float = exp(pct * 1.0/4.0)
  let investment = 100.0
  let quarterlyPayments: Contract = 
    let payment = qPct * investment
    All([
        flow(i, 30, payment, USD);
        flow(i, 60, payment, USD);    
        flow(i, 90, payment, USD);    
        flow(i, 120, payment, USD)])
  Then(quarterlyPayments, flow(i, maturityDate, investment, USD))

// Call option
let exampleEuropeanCallOption : Contract =
    let underlying = "DIKU A/S"
    let strike = 100.0 
    let maturity = 30
    let currency = USD
    let payoff = 
        Max(Value 0.0,
            Sub(Underlying(underlying, maturity), Value (strike * I i maturity)))
    Scale(payoff, One currency)

let exampleEuropeanPutOption : Contract =
    let underlying = "DIKU A/S"
    let strike = 100.0 
    let maturity = 30
    let currency = USD
    let payoff = 
        Max(Value 0.0, Sub(Value (strike * I i maturity), Underlying(underlying, maturity)))
    Scale(payoff, One currency)

let exampleForward : Contract =
    let underlying = "DIKU A/S"
    let strike = 100.0 
    let maturity = 30
    let currency = USD
    let payoff = Sub(Underlying(underlying, maturity), Value (strike * I i maturity))
    Scale(payoff, One currency)


let exampleCurrencySwap : Contract =
    let notional1 = 1000000.0
    let currency1 = USD
    let notional2 = 900000.0
    let currency2 = EUR
    let maturity = 30

    let leg1 = Scale(Value notional1, One currency1)
    let leg2 = Scale(Value notional2, One currency2)
    Then(leg1, Acquire(i, maturity, leg2))

let exampleFixedRateBond : Contract =
    let principal = 1000.0
    let rate = 0.05
    let maturity = 30
    let currency = USD
    let coupon = Scale(Value (principal * rate), One currency)
    let principalAtMaturity = Scale(Value principal, One currency)
    All [coupon; Acquire(i, maturity, principalAtMaturity)]
    

