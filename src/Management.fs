module Management
open Domain
open XMLFunctions

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
    | Then (c1, c2) ->
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
/// Retrieves a list of underlyings used in a given contract.
/// </summary>
/// <param name="c">The contract to retrieve the list of underlyings for.</param>
/// <param name="timeOffset">The time offset to apply to the underlyings.</param>
/// <returns>The names of underlyings used in the contract as a list of tuples (string * int), where the int represents the time.</returns>
let underlyings (c: Contract) : (string * int) list =
    let rec u (t : int) (c : Contract) : (string * int) list =
        match c with
        | One _ -> []
        | Scale (obs, c') ->
            let stocks_in_obs = underlyingsObs obs t
            let stocks_in_c = u t c'
            List.append stocks_in_obs stocks_in_c 
        | All cs -> List.concat (List.map (fun x -> u t c) cs) 
        | Acquire (days, c') -> u (t + days) c'
        | Give c' -> u t c'
        | Or (c1, c2) ->
            let stocks_in_c1 = u t c1
            let stocks_in_c2 = u t c2
            List.append stocks_in_c1 stocks_in_c2 
        | Then (c1, c2) ->
            let stocks_in_c1 = u t c1
            let stocks_in_c2 = u t c2
            List.append stocks_in_c1 stocks_in_c2 
    u 0 c |> List.distinct 



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


/// <summary>
/// Calculates the maturity date of a given contract.
/// </summary>
/// <param name="c">The contract to calculate the maturity date for.</param>
/// <returns>The maturity date of the contract.</returns>
let maturity (c : Contract) : int =
    let rec m (i : int) (c : Contract) : int =
        match c with
        | One _ -> i
        | Scale (obs, c1) -> max (obsMaturity i obs) (m i c1)
        | All [] -> i
        | All (c::cs) -> max (m i c) (m i (All cs))
        | Acquire (t, c1) -> m (t+i) c1
        | Or (c1, c2) -> max (m i c1) (m i c2)
        | Give c1 -> m i c1
        | Then (c1, c2) -> (m i c1) + (m i c2)
    m 0 c


let rec certainObs (o : Obs) : bool =
    match o with
    | Value _ -> true
    | Underlying _ -> false
    | Add (o1, o2) -> certainObs o1 && certainObs o2
    | Sub (o1, o2) -> certainObs o1 && certainObs o2
    | Mul (o1, o2) -> certainObs o1 && certainObs o2
    | Max (o1, o2) -> certainObs o1 && certainObs o2

let rec retrieveFlows (c : Contract) : Contract list =
    match c with
    | One _ -> []
    | Scale (obs, subContract) ->
        retrieveFlows subContract
    | All [] -> []
    | All contracts ->
        List.concat (List.map retrieveFlows contracts)
    | Acquire _ -> [c] // Include the Acquire flow in the list
    | Give subContract ->
        retrieveFlows subContract
    | Or (c1, c2) ->
        retrieveFlows c1 @ retrieveFlows c2
    | Then (c1, c2) ->
        retrieveFlows c1 @ retrieveFlows c2

let rec uncertainFlows (c : Contract) : Contract list =
    match c with
    | One _ -> []
    | Scale (obs, subContract) ->
        match certainObs obs with
        | true -> [] 
        | false -> uncertainFlows subContract
    | All contracts ->
        List.concat (List.map uncertainFlows contracts)
    | Acquire (_, Scale(Underlying _, _)) -> c :: uncertainFlows c
    | Acquire (_, Give(Scale(Underlying _, _))) -> c :: uncertainFlows c
    | Acquire _ -> []
    | Give subContract ->
        uncertainFlows subContract
    | Or (c1, c2) ->
        uncertainFlows c1 @ uncertainFlows c2
    | Then (c1, c2) ->
        uncertainFlows c1 @ uncertainFlows c2

let rec certainFlows (c : Contract) : Contract list =
    match c with
    | One _ -> []
    | Scale (obs, subContract) ->
        match certainObs obs with
        | true ->
            let k = certainFlows subContract
            if (k = []) then
                []
            else
                failwith "Contract is not simplified: We have a Scale(obs, certain c). That should just be c, since c is certain."
        | false -> [] 
    | All contracts -> List.concat (List.map certainFlows contracts)
    | Acquire (_, Scale(Underlying _, _)) -> []
    | Acquire (_, Give(Scale(Underlying _, _))) -> []
    | Acquire (_, cc) -> c :: certainFlows cc
    | Give subContract ->
        certainFlows subContract
    | Or (c1, c2) ->
        certainFlows c1 @ certainFlows c2
    | Then (c1, c2) ->
        certainFlows c1 @ certainFlows c2

let rec missingInformation(c : Contract) : Obs List =
    match c with
    | One _ -> []
    | Scale(o, c) ->
        match o with
        | Underlying(s,d) ->
            if d <= 0 then
                [Underlying(s,d)] @ missingInformation c
            else
                missingInformation c
        | _ -> missingInformation c
    | All cs -> List.concat (List.map missingInformation cs)
    | Acquire (t, Scale(Underlying(obs, d), _)) | Acquire (t, Give(Scale(Underlying(obs, d), _))) ->
        if t+d <= 0 then
            [Underlying(obs, t+d)]
        else
            []
    | Acquire(_, c) -> missingInformation c
    | Give c -> missingInformation c
    | Or (c1, c2) -> missingInformation c1 @ missingInformation c2
    | Then(c1, c2) -> missingInformation c1 @ missingInformation c2


let rec simplifyObs (E: (string * int) -> float) (o : Obs) : Obs =
  let simpl f opr o1 o2 : Obs =
    match (simplifyObs E o1,simplifyObs E o2) with
    | (Value r1, Value r2) -> Value(f r1 r2)
    | (o1, o2) -> opr(o1, o2)
  match o with
    | Value _ -> o
    | Underlying(s,t) ->
        try Value(E(s,t))
        with _ -> Underlying(s,t)
    | Mul (o1, o2) -> simpl (fun x y -> x*y) Mul o1 o2 
    | Add (o1, o2) -> simpl (fun x y -> x+y) Add o1 o2 
    | Sub (o1, o2) -> simpl (fun x y -> x-y) Sub o1 o2 
    | Max (o1, o2) -> simpl max Max o1 o2 

type flow =
    | Uncertain
    | Certain of double * Currency
    | Choose of (int * flow) List * (int * flow) List

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



// if Scale(o, certain c ) -> o Scale(o, evalc cetain c), we make it into an obs or smth lets try it :p 
let rec simplify (E: (string * int) -> float) (c: Contract) : Contract =
    match c with
    | All cs ->
        let cs = List.map (fun c -> simplify E c ) cs
        match cs with
        | [c] -> c
        | cs -> All cs
    | One _ -> c
    | Acquire(t, c) ->
        if t <= 0 then
            simplify E c
        else
            Acquire(t, (simplify E c))
    | Scale(k, c) ->
        match Scale(simplifyObs E k, simplify E c) with
        | Scale(k, Scale(kk, c1)) ->
            if (certainObs k && certainObs kk) then
                Scale(simplifyObs E (Obs.Mul(simplifyObs E k, simplifyObs E kk)), c1)
            else
                c1
        | Scale(_, All[]) -> All[]
        | Scale(k, c) ->
            let simpl = simplifyObs E k // im not sure if we need to simplify again
            if (simpl = Value 0.0) then All []
            else Scale(simpl, c)
        | _ -> Scale(simplifyObs E k, c) // im not sure if we need to simplify again
    | Give c ->
        match Give (simplify E c) with
        | Give (Give innerC) -> simplify E innerC 
        | innerC -> innerC
    | Or(c1, c2) -> Or(simplify E c1, simplify E c2)
    | Then(c1, c2) -> Then(simplify E c1, simplify E c2)


let adv (E : (string * int) -> float) (c : Contract) (d : int) : Contract =
    let rec advance (E : (string * int) -> float) (c : Contract) (d : int) : Contract =
        match c with
        | One _ -> c
        | Scale(o,c) -> Scale(o, advance E c d)
        | All(cs) -> All(List.map (fun c -> advance E c d) cs)
        | Acquire(t, c) -> Acquire(t-d, advance E c d)
        | Give(c) -> advance E c d
        | Or(c1, c2) -> Or(advance E c1 d, advance E c2 d)
    simplify E (advance E c d)
    (*




//let causal (c: Contract) : (int * string) List =
let causal (c : Contract) : (int * flow) List =
    let rec cs (t : int) (s : float Option) (c : Contract) : (int * flow) List =
        match c with
        | One ccy ->
            match s with
            | None -> [(t, Causal)]
            | Some s -> [(t, Causal(s, ccy))]
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
        | Or(c1, c2) -> failwith "to do "
    fl 0 (Some 1.0) c
    
    *)


let Then(c1 : Contract, c2 : Contract) : Contract = // acquire c2 as c1 expires.
    All[
        c1;
        Acquire(maturity(c1), c2)
    ]


let dummyE : (string * int) -> float =
    fun (s, t) ->
        match s, t with
        // dummy prices for testing
        | "DIKU", 0 -> 100.0
        | "DIKU", -1 -> 110.0
        | "DIKU", -2 -> 120.0
        | "DIKU", -3 -> 130.0
        | "DIKU", -4 -> 140.0
        | "DIKU", -5 -> 150.0
        | "DIKU", -6 -> 160.0
        | "DIKU", -7 -> 170.0
        | "DIKU", -8 -> 180.0
        | "DIKU", -9 -> 190.0
        | "DIKU", -10 -> 200.0
        | "AAPL", 0 -> 300.0
        | "AAPL", -1 -> 310.0
        | "AAPL", -2 -> 320.0
        | "AAPL", -3 -> 330.0
        | "AAPL", -4 -> 340.0
        | "AAPL", -5 -> 350.0
        | "AAPL", -6 -> 360.0
        | "AAPL", -7 -> 370.0
        | "AAPL", -8 -> 380.0
        | "AAPL", -9 -> 390.0
        | "AAPL", -10 -> 400.0
        | _, _ -> failwith "price not found"

   
    
