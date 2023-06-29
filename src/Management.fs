module Management
open Domain
open Instruments

/// <summary>
/// Calculates the maturity date of a given contract.
/// </summary>
/// <param name="c">The contract to calculate the maturity date for.</param>
/// <returns>The maturity date of the contract.</returns>
let maturity (c : Contract) : int =
    let rec m (i : int) (c : Contract) : int =
        match c with
        | One _ -> i
        | Scale (obs, c1) -> m i c1
        | All [] -> i
        | All (c::cs) -> max (m i c) (m i (All cs))
        | Acquire (t, c1) -> m (t+i) c1
        | Or (c1, c2) -> max (m i c1) (m i c2)
        | Give c1 -> m i c1
    m 0 c

type flow =
    | Uncertain
    | Certain of double * Currency
    | Choose of (int * flow) List * (int * flow) List
    | Causal of int


let flows (c : Contract) : (int * flow) List =
    let rec fl (t : int) (s : float Option) (c : Contract) : (int * flow) List =
        match c with
        | One ccy ->
            match s with
            | None -> [(t, Uncertain)]
            | Some s -> [(t, Certain(s, ccy))]
        | Scale(Value d,c) ->
            match s with 
            | None -> fl t None c
            | Some s -> fl t (Some (s*d)) c
        | Scale(_, c) -> fl t None c
        | Acquire(t', c) -> fl(t + t') s c
        | Give(c) ->
            match s with
            | None -> fl t None c
            | Some s -> fl t (Some (-s)) c
        | All cs -> List.concat (List.map (fl t s) cs)
        | Or(c1, c2) -> [(t, Choose((fl t s c1), fl t s c2))]
    fl 0 (Some 1.0) c


let causal (c : Contract) : (int * flow) List =
    let rec cs (t : int) (d : int) (s : float Option) (c : Contract) : (int * flow) List =
        match c with
        | One _ ->
            match s with
            | None -> []
            | Some s -> [t, Causal(d)]
        | Scale(Underlying(_,t'), c) ->
            match t' with
            | _ when t' > 0 -> cs t (d+t') (Some 1.0) c
            | _ -> cs t (d+t') None c
        | Scale(_, c) -> cs t d None c
        | Acquire(t', c) -> cs(t+t') (d+t') s c
        | All cs' -> List.concat (List.map (cs t d s) cs')
        | Give(c) -> cs t d s c
        | Or(c1, c2) -> cs t d s c1 @ cs t d s c2
    cs 0 0 None c

/// <summary>
/// Retrieves a list of underlyings used in a given contract.
/// </summary>
/// <param name="c">The contract to retrieve the list of underlyings for.</param>
/// <param name="timeOffset">The time offset to apply to the underlyings.</param>
/// <returns>The names of underlyings used in the contract as a list of tuples (string * int), where the int represents the time.</returns>
let underlyings (c: Contract) : (string * int) list =
    let rec underlyingsObs (t: int) (o: Obs)  : (string * int) list =
        match o with
        | Value _ -> []
        | Underlying (stock_name, time) -> [(stock_name, time + t)]
        | Mul (o1, o2) | Add (o1, o2) | Sub (o1, o2) | Max (o1, o2) -> 
            (underlyingsObs t o1) @ (underlyingsObs t o2) 
    let rec u (t : int) (c : Contract) : (string * int) list =
        match c with
        | One _ -> []
        | Scale (o, c') ->
            let u_o = underlyingsObs t o
            let u_c = u t c'
            u_o @ u_c
        | All cs -> List.concat (List.map (fun x -> u t x) cs) 
        | Acquire (d, c') -> u (t + d) c'
        | Give c' -> u t c'
        | Or (c1, c2) ->
            let u_c1 = u t c1
            let u_c2 = u t c2
            u_c1 @ u_c2 
    u 0 c |> List.distinct 


let rec simplifyObs (d : int) (E: (string * int) -> float) (o : Obs) : Obs =
  let simpl f opr o1 o2 : Obs =
    match (simplifyObs d E o1,simplifyObs d E o2) with
    | (Value r1, Value r2) -> Value(f r1 r2)
    | (o1, o2) -> opr(o1, o2)
  match o with
    | Value _ -> o
    | Underlying(s,t) ->
        try Value(E(s,t+d))
        with _ -> Underlying(s,t)
    | Mul (o1, o2) -> simpl (fun x y -> x*y) Mul o1 o2 
    | Add (o1, o2) -> simpl (fun x y -> x+y) Add o1 o2 
    | Sub (o1, o2) -> simpl (fun x y -> x-y) Sub o1 o2 
    | Max (o1, o2) -> simpl max Max o1 o2 

 
let simplify (E: (string * int) -> float) (c: Contract) : Contract =
    let rec si (d : int) (E: (string * int) -> float) (c: Contract) =
        match c with
        | All cs ->
            let cs = List.map (fun c -> si d E c ) cs
            match cs with
            | [c] -> c
            | cs -> All cs
        | One _ -> c
        | Acquire(t1, Acquire(t2, c'))
            -> si d E (Acquire(t1+t2, c'))
        | Acquire(t, c') ->
            if t <= 0 then
                si (d+t) E c'
            else
                Acquire(t, si (d+t) E c')
        | Scale(k, c) ->
            match Scale(simplifyObs d E k, si d E c) with
            | Scale(k, Scale(kk, c1)) ->
                    Scale(simplifyObs d E (Obs.Mul(simplifyObs d E k, simplifyObs d E kk)), c1)
            | Scale(_, All[]) -> All[]
            | Scale(k, c) ->
                let simpl = simplifyObs d E k 
                if (simpl = Value 0.0) then All []
                else Scale(simpl, c)
            | _ -> Scale(simplifyObs d E k, c) 
        | Give c ->
            let innerC = Give (si d E c)
            match innerC with
            | Give (Give c') -> si d E c'
            | innerC -> innerC
        | Or(c1, c2) -> Or(si d E c1, si d E c2)
    si 0 E c

let advance (E : (string * int) -> float) (d : int) (c : Contract)  : Contract =
    let rec adv (d : int) (c : Contract)  : Contract =
        match c with
        | One _ -> c
        | Scale(o,c) -> Scale(o, adv d c)
        | All(cs) -> All(List.map (fun x -> adv d x) cs)
        | Acquire(t, c) -> Acquire(t-d, c)
        | Give(c) -> Give(adv d c)
        | Or(c1, c2) -> Or(adv d c1, adv d c2)
    simplify E (adv d c)



let rec choose (f : Contract -> float) (c : Contract) : Contract =
    match c with
    | One _-> c
    | Scale(o, c) -> Scale(o, choose f c)
    | All(cs) -> All(List.map (fun c -> choose f c) cs)
    | Acquire(0, c) ->
        Acquire(0, choose f c)
    | Acquire(t, c) -> Acquire(t, c)
    | Give(c) -> Give(choose (fun x -> -f x) c)
    | Or(c1, c2) ->
        let (p1 : float option, p2 : float option) =
            try (Some(f (choose f c1)), Some(f (choose f c2)))
            with _ -> (None, None)
        match (p1, p2) with
        | (None, None) -> Or(choose f c1, choose f c2)
        | _ when p1 > p2 -> choose f c1
        | _ -> choose f c2

// for testing
let dummyE : (string * int) -> float =
    fun (s, t) ->
        match s, t with
        // dummy prices for testing
        | "AAPL", 0 -> 300.0
        | "MSFT", 0 -> 400.0
        | _, _ -> failwith "price not found"
   

// example
let cp =
    All[
    europeanCall2 1 "AAPL" 100.0 USD;
    flow 10 100.0 USD;
    Acquire(0, Or((flow 1 100.0 USD), (flow 1 110.0 USD)))
    ]




















(* Functions not a part of the report are below *)

let Then(c1 : Contract, c2 : Contract) : Contract = // acquire c2 as c1 expires.
    All[
        c1;
        Acquire(maturity(c1), c2)
    ]


/// <summary>
/// Finds the underlying stocks in a contract and returns them as an observable.
/// </summary>
/// <param name="obs">The contract to retrieve the list of stocks for.</param>
/// <returns>The underlying stocks as an observable list.</returns>
let rec getStocksAsObs (c: Contract) : Obs list =
    match c with
    | One _ -> []
    | Scale (obs, c') ->
        let rec extractUnderlying obs =
            match obs with
            | Underlying _ -> [obs]
            | Mul (obs1, obs2) -> List.append (extractUnderlying obs1) (extractUnderlying obs2)
            | Add (obs1, obs2) -> List.append (extractUnderlying obs1) (extractUnderlying obs2)
            | Sub (obs1, obs2) -> List.append (extractUnderlying obs1) (extractUnderlying obs2)
            | Max (obs1, obs2) -> List.append (extractUnderlying obs1) (extractUnderlying obs2)
            | _ -> []

        let obsList = extractUnderlying obs
        let stocks_in_c = getStocksAsObs c'
        List.append obsList stocks_in_c |> List.distinct
    | All cs -> List.map getStocksAsObs cs |> List.concat |> List.distinct
    | Acquire (_, c') -> getStocksAsObs c'
    | Give c' -> getStocksAsObs c'
    | Or (c1, c2) ->
        let stocks_in_c1 = getStocksAsObs c1
        let stocks_in_c2 = getStocksAsObs c2
        List.append stocks_in_c1 stocks_in_c2 |> List.distinct


/// <summary>
/// Retrieves a list of underlyings used in a given observable.
/// </summary>
/// <param name="obs">The observable to retrieve the list of underlyings for.</param>
/// <param name="timeOffset">The time offset to apply to the underlyings.</param>
/// <returns>The names of underlyings used in the observable as a list of tuples (string * int), where the int represents the time.</returns>
let rec underlyingsObs (obs: Obs) (t: int) : (string * int) list =
    match obs with
    | Value _ -> []
    | Underlying (stock_name, time) -> [(stock_name, time + t)]
    | Mul (obs1, obs2) | Add (obs1, obs2) | Sub (obs1, obs2) | Max (obs1, obs2) -> 
        let stocks_in_obs1 = underlyingsObs obs1 t
        let stocks_in_obs2 = underlyingsObs obs2 t
        List.append stocks_in_obs1 stocks_in_obs2 


/// <summary>
/// Finds the date of an underlying.
/// </summary>
/// <param name="c">The contract to calculate the maturity date for.</param>
/// <returns> The date of the underlying. </returns>
let rec obsMaturity (i : int) (o : Obs) : int =
    match o with
    | Value _ -> 0
    | Underlying (_, t) -> t+i
    | Mul(o1, o2) | Add (o1, o2) | Sub (o1, o2) | Max(o1, o2) ->
        max (obsMaturity i o1) (obsMaturity i o2)
    


let rec certainObs (o : Obs) : bool =
    match o with
    | Value _ -> true
    | Underlying _ -> false
    | Add (o1, o2) -> certainObs o1 && certainObs o2
    | Sub (o1, o2) -> certainObs o1 && certainObs o2
    | Mul (o1, o2) -> certainObs o1 && certainObs o2
    | Max (o1, o2) -> certainObs o1 && certainObs o2

