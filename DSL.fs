module DSL
open System
// Firstly, I need to define a languague that describes what a contract is. 
// It is basically an agreement between 2 parts to exchange something at a certain time. 
// The DSL is currently mostly inspired by the research article "Composing Contracts: An Adventure in Finanical Engineering, by Simon Peyton Jones, Jean-Marc Eber and Julian Seward"
type Currency = USD | EUR | DKK | None  
type Obs = 
  | Value of double
  | Underlying of double
  | Strike of double
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
  | Acquire of Contract
  | Give of Contract
  | And of Contract * Contract
  | Or of Contract * Contract
  | Truncate of DateTime * Contract // Trims the contracts horizon so that it cannot be acquired before the DateTime.
  | European of DateTime * Contract // you can acquire a underlying contract at time T 
  | Anytime of Contract // used for American options
  | Then of Contract * Contract // Then(c_1, c_2) means you acquire c_1 if it has not expired; else c2.

// Example 0: A contract cointaining nothing.
let c0 : Contract = Scale(Value(0.0), One((None)))
// Example 1: Zero Coupond Bond.  Recieve 100DKK on 22th of February 2000, if you acquire the contract before 22th of February 2000.
let c1 = Acquire(Truncate(DateTime(2000, 2, 22), Scale(Value(100.0), One(DKK))))
// Example 2: Zero Coupond Bond 2. Recieve 200USD on 1st of March 2000.
let c2 = Acquire(Truncate(DateTime(2000, 3, 1), Scale(Value(100.0), One(USD))))
// Example 3: A contract that involves c1 and c2.
let c3 = And(c1, c2)
// Example 4: A contract whose holder receives 100DKK at t=1 and pays 200USD at t=2.
let c4 = And(c1, Give(c2))
// Example 5: Gives the holder immediately 1EUR with infinite horizon.
let c5 = One(EUR)
// Example 6: Gives the holder immediately 100EUR with infinite horizon.
let c6 = Scale(Value(100.0), c5)
// Example 7: skipped
let c7 = One(USD)
// Example 8: European Contract. You can acquire v1 (Contract) on t if you want, but you need to give v2 (Contract). Can be money. Otherwise, nothing happens (c0)
let c8(t, v1 : Contract, v2 : Contract) = European(t, Or((And(v1, Give(v2))), c0))
// Example 9: American option.
let perhaps(t, u: Contract) = Truncate(t, Or(u, Scale(Value(0.0),One(None))))
//let c9(t1 : DateTime, t2 : DateTime) = Acquire(Truncate(t1))
// Define american contract here:
let american = 0

// Example n: Forward contract. Recieve S_T-K at time T; not before. Say, T = 22th of February 2000, S_T = 100DKK and K = 50DKK
let cn = Acquire(Truncate(DateTime(2000, 2, 22), Scale(Sub(Underlying(100.0), Strike(50.0)), One(DKK))))