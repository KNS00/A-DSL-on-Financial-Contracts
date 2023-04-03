module Analysis
open DSL
open FSharp.Stats
open FSharp.Math
open FSharp.Stats.Distributions

// Wiener Process
let WienerProcess(startTime : int, endTime : int, dt : int) : float list = 
    let normalDistribution = ContinuousDistribution.normal 0 1
    let numSteps = endTime - startTime //int ((endTime - startTime) / dt)
    let sampleValues = List.init numSteps (fun _ -> normalDistribution.Sample() * sqrt(float(dt)))
    let results = List.scan (+) 0.0 sampleValues
    results 

// Geometric Brownian Motion
let GeometricBrownianMotion (currentPrice : float, startTime : int, endTime : int, dt : int, mu: float, sigma: float, wpValues : float list) : float list =
    //let wpValues : float list = WienerProcess (startTime, endTime, dt)
    let t = [startTime .. dt .. endTime]
    let numSteps = 1 //int ((endTime - startTime) / dt)
    let sampleValues = 
        List.zip wpValues t |> List.map (fun (w,t) -> 
        let drift = (mu - 0.5 * sigma**2) * float(t)
        let diffusion = sigma * w
        currentPrice * exp(drift + diffusion)
        )   
    sampleValues

// Get interest rate (constant for now, fix later)
let I (i : int) : float = // assume 0.02
    exp(0.02 * float(i)/365.0) //

// Get maturity date of contract
let rec getMaturityDate (c : Contract) : int =
    match c with
    | One _ -> 0
    | Scale (obs, c1) -> getMaturityDate c1
    | All [] -> 0
    | All (c1::cs) -> max (getMaturityDate c1) (getMaturityDate (All cs))
    | Acquire (i, c1) -> i
    | Or (c1, c2) -> max (getMaturityDate c1) (getMaturityDate c2)
    | Give c1 -> getMaturityDate c1
    | Anytime c1 -> getMaturityDate c1
    | Then (c1, c2) -> max (getMaturityDate c1) (getMaturityDate c2)


// function to evaluate observables
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



// function to evaluate contracts
let rec evalc (I:int->float) (E:(string*int)->float) (c: Contract) : float =
  match c with
  | One _ -> 1.0 // only working with one currency for now, fix later
  | Scale (obs, c1) -> evalo E obs * evalc I E c1 
  | All [] -> 0.0
  | All (c1::cs) -> evalc I E c1 + evalc I E (All cs)
  | Acquire(i, c1) -> I(i) * evalc I E c1 
  | Or(c1, c2) -> evalc I E c1 + evalc I E c2 
  | Give(c1) -> -1.0 * evalc I E c1 
  | Anytime(c1) -> evalc I E c1 
  | Then(c1, c2) -> if getMaturityDate(c1) > 0 then evalc I E c1 else evalc I E c2

// find stock price, needs fix
let rec E(name: string, t: int) : float =  
    match t with
    | t when t >= 1 -> 
        (*let current_price = E(name, 0)
        let mu = 0.1
        let sigma = 1.0 
        List.init 1000000 (fun _ -> GBM(current_price, mu, sigma, t))
        |> List.average
        100.0 *)
        E(name, 0) * exp((0.0 - 0.2**2.0 / 2.0) * float t) // lets look up the parameters maybe?? how should this be implemented
    | t when t <= 0 -> 100.0 // get price from api, csv or alike


let simStocks (c : Contract) (stocks : (string * float * float * float) list) : (string * (int * float) list) list = // stocks is name * mu * sigma. This function lets you simulate prices for mulitple stocks
    let simulate (currentPrice: float) (days : int) (mu : float) (sigma : float) (wpValues : float list) : (int * float) list = // returns days * prices. 
        let dates : int list = [0 .. 1 .. days]
        let GBM : float list = GeometricBrownianMotion(currentPrice, 0, days, 1, mu, sigma, wpValues)
        List.map2 (fun d p -> (d, p)) dates GBM

    let wpValues = WienerProcess(0, (getMaturityDate c), 1)
    // 1 because we are assuming day time increments
    let sim : (string * (int * float) list) list = // simulate stock prices
        List.map (fun (s, S0, mu, sigma) -> (s,simulate S0 (getMaturityDate c) mu sigma wpValues)) stocks  
    sim


let mc1(c : Contract) (stocks : (string * float * float * float) list) : float =  
    let data = simStocks c stocks 
    let E(s,n) : float = 
        let stockdata = List.find(fun (s',_) -> s = s') data |> snd
        let quote = List.find(fun(n',_) -> n = n' ) stockdata |> snd
        quote
    evalc I E c

// simulate stock prices sims amount of times and take the average (MC simulation)
let mc (c : Contract) (sims : int) (stocks : (string * float * float * float) list) : float =  
    let rec loop (i: int) (acc : float) : float =  // using recursion because otherwise we have to change seed everytime
        if i = sims then acc
        else loop (i + 1) (mc1 c stocks + acc)
    loop 0 (float sims) / float sims
