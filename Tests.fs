module Tests
open Analysis
open DSL
let flow(i: int, v: double, c: Currency) : Contract = Acquire(i,Scale(Value(v), One(c)))  
let contract1 = One DKK // A contract containing the valuta DKK
let contract2 = Scale(Value(100.0), One(EUR)) // A contract containing 100 EUR
let contract3 = flow(5, 10.0, DKK) // A contract containing the acquisition of 10 DKK, 5 days from now.
let contract4 = flow(365, 100.0, EUR) // A contract containing the acquisition of 100 EUR 365 days from now.
let contract5 = All([contract1; contract2; contract3; contract4])

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
        Acquire(maturity, Scale(obs, One EUR))) // 1.0 * Acquire(5, 115 EUR)

let TestStockPrice = E(equity, maturity)
let result1 = evalc I E contract1
let result2 = evalc I E contract2
let result3 = evalc I E contract3
let result4 = evalc I E contract4
let result5 = evalc I E contract5
let result6 = evalc I E call_option
printfn "%s %f" "Test E function:" TestStockPrice 
printfn "%s %A" "Test Evaluation of contract1:" result1 // 1.0,     correct
printfn "%s %A" "Test Evaluation of contract2:" result2 // 100.0,   correct
printfn "%s %A" "Test Evaluation of contract3:" result3 // 9.99,   correct
printfn "%s %A" "Test Evaluation of contract4:" result4 // 98.02,  correct
printfn "%s %A" "Test Evaluation of contract5:" result5 // 209.02,   correct
printfn "%s %A" "Test Evaluation of call option:" result6 // Should be around 115 beacuse E[S_T] = 165 and 165 - 50 = 115. However, this will
                                                          // not work for exotic options! Because E[F(S_T)] != F(E[S_T]) for non-linear functions!