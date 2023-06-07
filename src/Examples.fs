module Examples
open Domain


let flow(t: int) (v: double) (c: Currency) : Contract
    = Acquire(t, Scale(Value v, One c))

let zcb (T : int) (face : float) (ccy : Currency) : Contract = 
  flow T face ccy

// the maturity input is in days
let cb (T : int) (face : float) (rate : float) (yearlyFreq : float) (ccy : Currency) : Contract =
    let dates = int(yearlyFreq * 365.)
    let rateDates = [dates .. dates .. T] |> List.ofSeq
    let couponFlows : Contract List = List.map (fun x -> flow x (face * rate / yearlyFreq) ccy) rateDates
    let faceFlow = zcb T face ccy
    All(faceFlow :: couponFlows)

let ccSwap (T : int) (ccA : Currency) (ccB : Currency) (nA : float) (nB : float) (rA : float) (rB : float) (yearlyFreq : float) : Contract =
    let dates = int(yearlyFreq * 365.)
    let rateDates = [365 / dates .. 365  .. T]
    let interestPaymentsA = List.map (fun x -> Give(flow x (nA * rA / yearlyFreq) ccA)) rateDates
    let interestPaymentsB = List.map (fun x -> flow x (nB * rB / yearlyFreq) ccB) rateDates
    All([
        Give(flow 0 nA ccA); // Inception: give nA units of currency ccA.
        flow 0 nB ccB;       // Inception: acquire nB units of currency ccB.
        All(interestPaymentsA); // Pay interest at intervals
        All(interestPaymentsB); // Receive interest at intervals
        Give(flow T nB ccB); // At maturity, pay back nB units of currency ccB.
        flow T nA ccA        // At maturity, acquire back nA units of currency ccA.
    ])


let europeanCall1 (T : int) (stock : string) (strike : float )  (ccy : Currency) : Contract =
    let payoff : Obs = 
        Max(Value 0.0,
            Sub(Underlying(stock, 0), 
                Value strike))
    Acquire(T, Scale(payoff, One ccy))

let europeanCall2 (T : int) (stock : string) (strike : float )  (ccy : Currency) : Contract =
    let c : Contract = Or(
                        Scale(Value 0.0, One ccy),
                        Scale(Sub(Underlying(stock, 0),Value strike), One USD))
    Acquire(T, c)



let europeanPut (T : int) (u : string) (strike : float )  (ccy : Currency) : Contract =
    let payoff = 
        Max(Value 0.0,
            Sub(Value strike,
                Underlying(u, 0)))
    Acquire(T, Scale(payoff, One ccy))

let forwardOption (T : int) (u : string) (strike : float )  (ccy : Currency)  : Contract =
    let payoff = 
        Sub(Value strike,
            Underlying(u, 0))
    Acquire(T, Scale(payoff, One ccy))

let chooserOption(t : int) (T : int) (stock : string) (strike : float) (ccy : Currency) : Contract =
    let ec = europeanCall1 T stock strike ccy
    let ep = europeanPut T stock strike ccy
    Acquire(t, Or(ec, ep)) // remember: the maturity of ec or ep is then t+T.
