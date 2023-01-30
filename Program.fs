module Program
open DSL
open Analysis


(*
// Define a function to calculate the value of a contract
let value (c: Contract) : int =
    match c with
    | One (curr) ->
        match curr with
        | USD -> 1
        | EUR -> 2
        | GBP -> 3
    | Underlying intVal -> intVal
    | Fixing_date _ -> 0
    | Maturity_date _ -> 0
    | Currency _ -> 0
*)





(*
type contract(underlying : string, fixing_date : DateTime, maturity_date : DateTime, currency : string ) = 
  member val underlying : string = underlying
  member val fixing_date = fixing_date
  member val maturity_date = maturity_date
  member val currency = currency
  
  member this.getUnderlying() = this.underlying
  member this.getFixingDate() = this.fixing_date
  member this.getMaturityDate() = this.maturity_date
  member this.getCurrency() = this.currency

  type europeanCall (underlying : string, fixing_date : DateTime, maturity_date : DateTime, currency : string, strike : double, automatic : bool) = 
    inherit contract(underlying, fixing_date, maturity_date, currency)
    member val strike = strike 
    member val automatic = automatic

    member this.getStrike() = this.strike
    member this.getAutomatic() = this.automatic

  type europeanPut (underlying : string, fixing_date : DateTime, maturity_date : DateTime, currency : string, strike : double, automatic : bool) = 
      inherit contract(underlying, fixing_date, maturity_date, currency)
      member val strike = strike
      member val automatic = automatic

      member this.getStrike() = this.strike
      member this.getAutomatic() = this.automatic




let createEuropeanPutExample (underlying : string, fixing_date : DateTime, maturity_date : DateTime, currency : string, strike : double, automatic : bool) =
  let put = europeanPut(underlying, fixing_date, maturity_date, currency, strike, automatic)
  put

let putExample = createEuropeanPutExample(
  "Caféen A/S",
  new DateTime(2022, 12, 10),
  new DateTime(2023, 12, 10),
  "DKK",
  100.0,
  true
)
printfn "Strike: %f %s " (putExample.getStrike()) (putExample.getCurrency())  
*)