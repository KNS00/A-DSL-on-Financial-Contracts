module Evaluations
open Domain
/// <summary>
/// Evaluates a currency to USD.
/// </summary>
/// <param name="ccy">The currency to evaluate.</param>
/// <returns>The evaluated currency value as a float.</returns>
let ccy (curr: Currency) : float =
    let EURUSD = 1.10
    let GBPUSD = 1.24
    let DKKUSD = 0.15
    match curr with
    | USD -> 1.0
    | EUR -> EURUSD
    | GBP -> GBPUSD
    | DKK -> DKKUSD
    | _ -> failwith "Currency not found"

/// <summary>
/// Evaluates an observable value using a given function.
/// </summary>
/// <param name="E">The function used to evaluate stock prices.</param>
/// <param name="o">The observable to evaluate.</param>
/// <returns>The evaluated observable value as a float.</returns>
let rec evalo (E:(string*int)->float) (o : Obs) : float = 
  match o with
  | Value n -> n
  | Underlying (s, t) -> E(s,t)
  | Mul (c1, c2) ->
      let n1 = evalo E c1
      let n2 = evalo E c2
      n1 * n2
  | Add (c1, c2) ->
      let n1 = evalo E c1
      let n2 = evalo E c2
      n1 + n2
  | Sub (c1, c2) ->
      let n1 = evalo E c1
      let n2 = evalo E c2
      n1 - n2
  | Max (c1, c2) ->
      let n1 = evalo E c1
      let n2 = evalo E c2
      max (n1) (n2)

/// <summary>
/// Evaluates a given contract.
/// </summary>
/// <param name="I">The interest rate to use for evaluation.</param>
/// <param name="E">The function used to evaluate stock prices.</param>
/// <param name="c">The contract to evaluate.</param>
/// <returns>The evaluated contract as a float.</returns>
let rec evalc (I:int->float) (E:(string*int)->float) (c: Contract) : float =
  match c with
  | One c -> ccy c // evaluate currency
  | Scale (obs, c1) -> evalo E obs * evalc I E c1 
  | All [] -> 0.0
  | All (c1::cs) -> evalc I E c1 + evalc I E (All cs)
  | Acquire(t, c1) -> I t * evalc I E c1 
  | Or(c1, c2) -> evalc I E c1 + evalc I E c2 
  | Give(c1) -> -1.0 * evalc I E c1 
  | Anytime(c1) -> evalc I E c1 
  | Then(c1, c2) -> if getMaturityDate(c1) > 0 then evalc I E c1 else evalc I E c2
