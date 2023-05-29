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
    | Acquire (_, _, c') -> getStocksAsObs c'
    | Give c' -> getStocksAsObs c'
    | Or (c1, c2) ->
        let stocks_in_c1 = getStocksAsObs c1
        let stocks_in_c2 = getStocksAsObs c2
        List.append stocks_in_c1 stocks_in_c2 |> List.distinct
    | Then (c1, c2) ->
        let stocks_in_c1 = getStocksAsObs c1
        let stocks_in_c2 = getStocksAsObs c2
        List.append stocks_in_c1 stocks_in_c2 |> List.distinct

(*
/// <summary>
/// Finds the name and date of an underlying.
/// </summary>
/// <param name="obs">The underlying to retrieve the name and date of..</param>
/// <returns>The underlying stock as a observable.</returns>
let getUnderlyingInfo(o : Obs) : string * int =
    match o with
    | Underlying (name, t) -> (name, obsMaturity o)
    | _ -> failwith "not an underlying"
    *)
let rec evaloo (E: (string * int) -> float) (o : Obs) : float =
    match o with
    | Value n -> n
    | Underlying (s, t) -> E(s,t)
    | Mul (c1, c2) ->
        let n1 = evaloo E c1
        let n2 = evaloo E c2
        n1 * n2
    | Add (c1, c2) ->
        let n1 = evaloo E c1
        let n2 = evaloo E c2
        n1 + n2
    | Sub (c1, c2) ->
        let n1 = evaloo E c1
        let n2 = evaloo E c2
        n1 - n2
    | Max (c1, c2) ->
        let n1 = evaloo E c1
        let n2 = evaloo E c2
        max n1 n2


/// <summary>
/// Retrieves a list of stocks used in a given contract.
/// </summary>
/// <param name="c">The contract to retrieve the list of stocks for.</param>
/// <returns>The names of stocks used in the contract as a list of strings.</returns>
let rec underlyings (c: Contract) : string list =
    match c with
    | One _ -> []
    | Scale (obs, c') ->
        let stocks_in_obs = underlyingsObs obs
        let stocks_in_c = underlyings c'
        List.append stocks_in_obs stocks_in_c |> List.distinct |> List.sort
    | All cs -> List.map underlyings cs |> List.concat |> List.distinct |> List.sort
    | Acquire (_, _, c') -> underlyings c'
    | Give c' -> underlyings c'
    | Or (c1, c2) ->
        let stocks_in_c1 = underlyings c1
        let stocks_in_c2 = underlyings c2
        List.append stocks_in_c1 stocks_in_c2 |> List.distinct |> List.sort
    | Then (c1, c2) ->
        let stocks_in_c1 = underlyings c1
        let stocks_in_c2 = underlyings c2
        List.append stocks_in_c1 stocks_in_c2 |> List.distinct |> List.sort

/// <summary>
/// Retrieves a list of stocks used in a given observable value.
/// </summary>
/// <param name="obs">The observable value to retrieve the list of stocks for.</param>
/// <returns>The names of stocks used in the observable as a list of strings.</returns>
and underlyingsObs (obs: Obs) : string list =
    match obs with
    | Value _ -> []
    | Underlying (stock_name, _) -> [stock_name]
    | Mul (obs1, obs2) ->
        let stocks_in_obs1 = underlyingsObs obs1
        let stocks_in_obs2 = underlyingsObs obs2
        List.append stocks_in_obs1 stocks_in_obs2 |> List.distinct |> List.sort
    | Add (obs1, obs2) ->
        let stocks_in_obs1 = underlyingsObs obs1
        let stocks_in_obs2 = underlyingsObs obs2
        List.append stocks_in_obs1 stocks_in_obs2 |> List.distinct |> List.sort
    | Sub (obs1, obs2) ->
        let stocks_in_obs1 = underlyingsObs obs1
        let stocks_in_obs2 = underlyingsObs obs2
        List.append stocks_in_obs1 stocks_in_obs2 |> List.distinct |> List.sort
    | Max (obs1, obs2) ->
        let stocks_in_obs1 = underlyingsObs obs1
        let stocks_in_obs2 = underlyingsObs obs2
        List.append stocks_in_obs1 stocks_in_obs2 |> List.distinct |> List.sort

/// <summary>
/// Finds the date of an underlying.
/// </summary>
/// <param name="c">The contract to calculate the maturity date for.</param>
/// <returns> The date of the underlying. </returns>
let rec obsMaturity (o : Obs) : int =
    match o with
    | Value _ -> 0
    | Underlying (_, t) -> t
    | Mul (o1, o2) -> max (obsMaturity o1) (obsMaturity o2)
    | Add (o1, o2) -> max (obsMaturity o1) (obsMaturity o2)
    | Sub (o1, o2) -> max (obsMaturity o1) (obsMaturity o2)
    | Max (o1, o2) -> max (obsMaturity o1) (obsMaturity o2)


/// <summary>
/// Calculates the maturity date of a given contract.
/// </summary>
/// <param name="c">The contract to calculate the maturity date for.</param>
/// <returns>The maturity date of the contract.</returns>
let rec maturity (c : Contract) : int =
    match c with
    | One _ -> 0
    | Scale (obs, c1) -> max (obsMaturity obs) (maturity c1)
    | All [] -> 0
    | All (c1::cs) -> max (maturity c1) (maturity (All cs))
    | Acquire (_, t, c1) -> max t (maturity c1)
    | Or (c1, c2) -> max (maturity c1) (maturity c2)
    | Give c1 -> maturity c1
    | Then (c1, c2) -> max (maturity c1) (maturity c2)


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
    | Acquire (_, _, Scale(Underlying _, _)) -> c :: uncertainFlows c
    | Acquire (_, _, Give(Scale(Underlying _, _))) -> c :: uncertainFlows c
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
    | Acquire (_, _, Scale(Underlying _, _)) -> []
    | Acquire (_, _, Give(Scale(Underlying _, _))) -> []
    | Acquire (_, _, cc) -> c :: certainFlows cc
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
    | Acquire (_, _, Scale(Underlying(obs, d), _)) | Acquire (_, _, Give(Scale(Underlying(obs, d), _))) ->
        if d <= 0 then
            [Underlying(obs, d)]
        else
            []
    | Acquire(_, _, c) -> missingInformation c
    | Give c -> missingInformation c
    | Or (c1, c2) -> missingInformation c1 @ missingInformation c2
    | Then(c1, c2) -> missingInformation c1 @ missingInformation c2


let rec simplifyObs (E: (string * int) -> float) (o : Obs) : Obs =
  let simpl opr o1 o2 : Obs = 
    opr(simplifyObs E o1,simplifyObs E o2)
  try Value(evaloo E o)
  with _ ->
    match o with
    | Value _ -> o
    | Underlying(s,t) ->
        try Value(E(s,t))
        with _ -> Underlying(s,t)
 (*       if (t <= 0) then
            try
                Value(E(s,t))
            with _ ->
                Underlying(s,t)
        else
            Underlying(s,t)*)
    | Mul (o1, o2) -> simpl Mul (simplifyObs E o1) (simplifyObs E o2)
    | Add (o1, o2) -> simpl Add (simplifyObs E o1) (simplifyObs E o2)
    | Sub (o1, o2) -> simpl Sub (simplifyObs E o1) (simplifyObs E o2)
    | Max (o1, o2) -> simpl Max (simplifyObs E o1) (simplifyObs E o2)




// if Scale(o, certain c ) -> o Scale(o, evalc cetain c), we make it into an obs or smth lets try it :p 
let rec simplify (E: (string * int) -> float) (c: Contract) : Contract =
    match c with
    | All cs ->
        let cs = List.map (fun c -> simplify E c ) cs
        match cs with
        | [c] -> c
        | cs -> All cs
    | One _ -> c
    | Acquire(i, t, c) ->
        if t <= 0 then
            simplify E c
        else
            Acquire(i, t, (simplify E c))
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
        let rec subtractDate d obs =
            match obs with
            | Value _ -> obs
            | Underlying (symbol, t) -> Underlying (symbol, t - d)
            | Mul (o1, o2) -> Mul (subtractDate d o1, subtractDate d o2)
            | Add (o1, o2) -> Add (subtractDate d o1, subtractDate d o2)
            | Sub (o1, o2) -> Sub (subtractDate d o1, subtractDate d o2)
            | Max (o1, o2) -> Max (subtractDate d o1, subtractDate d o2)
        match c with
        | One _ -> c
        | Scale(o,c) ->
            match o with
            | _ -> Scale(subtractDate d o, advance E c d)
        | All(cs) -> All(List.map (fun c -> advance E c d) cs)
        | Acquire(i, t, c) -> Acquire(i, t-d, advance E c d)
        | Give(c) -> advance E c d
        | Then(c1, c2) -> Then(advance E c1 d, advance E c2 d)
        | Or(c1, c2) -> Or(advance E c1 d, advance E c2 d)
    simplify E (advance E c d)


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

   
    
let contractio =
    All [
        //One DKK;
        //One DKK;
        //Scale(Value 100.0, One USD);
        //Acquire(0.02, 10, Scale(Underlying("AAPL", -3), One USD));
        Acquire(0.02, -3, Give(Scale(Underlying("AAPL", -3), One USD)))
        //Give(Give(One DKK))
        //Scale(Value 100.0, Acquire(0.02, 10, Scale(Value 100.0, One USD)));
        //Acquire(0.02, 10, Give(Scale(Value 100.0, One USD)));
        //Acquire(0.02, 10, Give(Scale(Underlying("AAPL", -3), One USD)))
    ]

let cflows = simplify dummyE (contractio)