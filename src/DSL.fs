module DSL
open System
// Firstly, I need to define a languague that describes what a contract is. 
// It is basically an agreement between 2 parts to exchange something at a certain time. 
// The DSL is currently mostly inspired by the research article "Composing Contracts: An Adventure in Finanical Engineering, by Simon Peyton Jones, Jean-Marc Eber and Julian Seward"
// and Martin Elsmans FP for Trade Management presentation (2010, Simcorp)
type Currency = USD | EUR | GDP | DKK | None

type Obs = 
  | Value of double // how many
  | Underlying of string * int // get stock price at specific time relative to today
  | Mul of Obs * Obs
  | Add of Obs * Obs
  | Sub of Obs * Obs
  | Max of Obs * Obs


type Contract = // All these constructers are used to define a contract. 
  | One of Currency
  | Scale of Obs * Contract 
  | All of Contract list
  | Acquire of int * Contract
  | Give of Contract // need datetime aswell here imo. Keep in mind that this is not the ACTION of giving. It is simply just a part of defining a contract.
  | Or of Contract * Contract
  | Anytime of Contract // used for American options
  | Then of Contract * Contract // Then(c1, c2) means you acquire c1 if it has not expired; else c2.
