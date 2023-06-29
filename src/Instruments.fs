module Instruments
open Domain

/// <summary>
/// An instrument representing a flow.
/// </summary>
/// <param name="t">The time at which the flow is executed.</param>
/// <param name="v">The amount of currency being transfered.</param>
/// <param name="c">The currency the amount is transfered in.</param>
/// <returns>A contract representing a flow.</returns>
let flow(t: int) (v: double) (c: Currency) : Contract
    = Acquire(t, Scale(Value v, One c))


/// <summary>
/// An instrument representing a zero coupon bond.
/// </summary>
let zcb (T : int) (face : float) (ccy : Currency) : Contract = 
  flow T face ccy



/// <summary>
/// An instrument representing a coupon bond.
/// </summary>
let cb (T : int) (face : float) (rate : float) (yearlyFreq : float) (ccy : Currency) : Contract =
    let dates = int(yearlyFreq * 365.)
    let rateDates = [dates .. dates .. T] |> List.ofSeq
    let couponFlows : Contract List = List.map (fun x -> flow x (face * rate) ccy) rateDates
    let faceFlow = zcb T face ccy
    All(faceFlow :: couponFlows)

/// <summary>
/// An instrument representing a Cross Currency Swap
/// </summary>
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



/// <summary>
/// An instrument representing a European Call Option using the payoff function for the option. 
/// </summary>
let europeanCall1 (T : int) (stock : string) (strike : float )  (ccy : Currency) : Contract =
    let payoff : Obs = 
        Max(Value 0.0,
            Sub(Underlying(stock, 0), 
                Value strike))
    Acquire(T, Scale(payoff, One ccy))

/// <summary>
/// An instrument representing a European Call Option using the Or constructor indicating a choice (S_T - K)^+.
/// </summary>
let europeanCall2 (T : int) (stock : string) (strike : float )  (ccy : Currency) : Contract =
    let c : Contract = Or(
                        Scale(Value 0.0, One ccy),
                        Scale(Sub(Underlying(stock, 0),Value strike), One ccy))
    Acquire(T, c)



/// <summary>
/// An instrument representing a European Put Option.
/// </summary>
let europeanPut (T : int) (stock : string) (strike : float )  (ccy : Currency) : Contract =
    let c : Contract = Or(
                        Scale(Value 0.0, One ccy),
                        Scale(Sub(Value strike, Underlying(stock, 0)), One ccy))
    Acquire(T, c)

/// <summary>
/// An instrument representing a Forward Option.
/// </summary>
let forwardOption (T : int) (stock : string) (strike : float )  (ccy : Currency)  : Contract =
    let payoff = 
        Sub(Value strike,
            Underlying(stock, 0))
    Acquire(T, Scale(payoff, One ccy))

/// <summary>
/// An instrument representing a Chooser Option, which here is a choice between a european call or european put.
/// </summary>
let chooserOption(t : int) (T : int) (stock : string) (strike : float) (ccy : Currency) : Contract =
    let ec = europeanCall2 T stock strike ccy
    let ep = europeanPut T stock strike ccy
    Acquire(t, Or(ec, ep)) // remember: the maturity of ec or ep is then t+T.