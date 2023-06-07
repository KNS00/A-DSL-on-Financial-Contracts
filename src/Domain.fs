module Domain

/// <summary>
/// Represents a currency.
/// </summary>
type Currency = USD | EUR | GBP | DKK 

/// <summary>
/// Represents an observable value: a value that is agreed upon by two parties.
/// </summary>
type Obs = 
  | Value of double // how many
  | Underlying of string * int // get stock price at specific time relative to today
  | Mul of Obs * Obs
  | Add of Obs * Obs
  | Sub of Obs * Obs
  | Max of Obs * Obs

/// <summary>
/// Represents a financial contract.
/// </summary>
type Contract = // All these constructers are used to define a contract. 
  | One of Currency
  | Scale of Obs * Contract 
  | All of Contract List
  | Acquire of int * Contract // Acquire contract int days from now assuming a constant interest rate
  | Give of Contract 
  | Or of Contract * Contract