module DSL
open System
// Firstly, I need to define a languague that describes what a contract is. 
// It is basically an agreement between 2 parts to exchange something at a certain time. 
// The DSL is currently mostly inspired by the research article "Composing Contracts: An Adventure in Finanical Engineering, by Simon Peyton Jones, Jean-Marc Eber and Julian Seward"
type Currency = USD | EUR | GDP | DKK | None

type Obs = 
  | Value of double // how many
  | Underlying of string * DateTime
  | Strike of double
  | AveragePrice of double // used for Asian Contracts
  | Mul of Obs * Obs
  | Add of Obs * Obs
  | Sub of Obs * Obs
  | Max of Obs * Obs

type Contract =
  | One of Currency
  | Scale of Obs * Contract 
  | All of Contract list
  | FixingDate of DateTime
  | MaturityDate of DateTime   
  | Acquire of DateTime * Contract
  | Give of Contract // need datetime aswell here imo
  | Or of Contract * Contract
 // | Truncate of DateTime * Contract // Trims the contracts horizon so that it cannot be acquired before the date.
 // Tbh I dont really see right now why Truncate is needed.
  | Anytime of Contract // used for American options
  | Then of Contract * Contract // Then(c1, c2) means you acquire c1 if it has not expired; else c2.


// Example 0: A contract cointaining nothing. That is: A value of 0 with no currency.
let c0: Contract =  Scale(Value(0.0), One((None)))
let c0_: Contract = All [] 
let flow(d: string, v: double, c: Currency) : Contract = Acquire(System.DateTime.Parse d,Scale(Value(v), One(c)))  
(* Acquire a bond that pays 100 USD on the 1st of December 2023. *)
let zcb1: Contract = 
  let maturityDate: string = "2000-1-12"
  flow(maturityDate, 100.0, USD)

(* Example: Acquire a bond that pays 100 USD on the 1st of December 2023 and pays dividends of 10 USD on the 1st of every quarter. *)
let dividendPayingBond: Contract = 
  let maturityDate: string = "2000/12/1"
  let pct: float = 0.02
  let qPct : float = exp(pct * 1.0/4.0)
  let investment: float = 100.0
  let quarterlyPayments: Contract = 
    let payment: float = qPct * investment
    All([
        flow("2000/3/1", payment, USD);
        flow("2000/6/1", payment, USD);    
        flow("2000/9/1", payment, USD);    
        flow("2000/12/1", payment, USD)])
  Then(quarterlyPayments, flow(maturityDate, investment, USD))

// Example 2: Zero Coupond Bond 2. Recieve 200USD on 1st of March 2000.
let zcb2: Contract = Acquire(DateTime(2000, 3, 1), Scale(Value(100.0), One(USD)))
// Example 3: A contract that involves zcb1 and c2.
let c3: Contract = All([zcb1; zcb2])
// Example 4: A contract whose holder receives 100DKK on 22th of February 2000 and pays 200USD at 1st of March 2000.
let c4: Contract = All([zcb1; Give(zcb2)])
// Example 8: 10 European Call Option acquired 2000/2/22, with expiry date 2000/3/1 with Vestas as underlying and a strike of 100.0.
let europeanCall: Contract =
  let acquireDate: DateTime = DateTime(2000, 2, 22)
  let expiryDate: DateTime = DateTime(2000, 3, 1)
  let equity: string = "Vestas Wind Systems A/S"
  let strike: float = 100.0
  let nominal: float = 10.0
  let obs: Obs = Max(Value(0.0), Sub(Underlying(equity, expiryDate), Value(strike)))
  Scale(Value(nominal), Acquire(expiryDate,Scale(obs,One(DKK))))

let americanCall: Contract = 
  let acquireDate: DateTime = DateTime(2000, 2, 22)
  let expiryDate: DateTime = DateTime(2000, 3, 1)
  let equity: string = "Vestas Wind Systems A/S"
  let strike: float = 100.0
  let units: float = 10.0
  let obs: Obs = 
    Max(Value(0.0), Sub(Underlying(equity, expiryDate), Value(strike)))
  Anytime(Acquire(expiryDate, Scale(Value(units), Scale(obs, One(DKK)))))

let forward: Contract = 
  let acquireDate: DateTime = DateTime(2000, 2, 22)
  let expiryDate: DateTime = DateTime(2000, 3, 1)
  let equity: string = "Vestas Wind Systems A/S"
  let price: float = 100.0
  let nominal: float = 10.0
  let obs: Obs = Sub(Underlying(equity, expiryDate), Value(price))
  Scale(Value(nominal), Acquire(expiryDate, Scale(obs,One(DKK))))

// Example n: Forward contract. Recieve S_T-K at time T; not before. Say, T = 22th of February 2000, S_T = 100DKK and K = 50DKK
