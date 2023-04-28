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
  | All of Contract list
  | Acquire of int * Contract // Acquire contract int days from now
  | Give of Contract // need datetime aswell here imo. Keep in mind that this is not the ACTION of giving. It is simply just a part of defining a contract.
  | Or of Contract * Contract
  | Then of Contract * Contract // Then(c1, c2) means you acquire c1 if it has not expired; else c2.

/// <summary>
/// Finds the date of an underlying.
/// </summary>
/// <param name="c">The contract to calculate the maturity date for.</param>
/// <returns> The date of the underlying. </returns>
let rec getObsMaturityDate (o : Obs) : int =
    match o with
    | Value _ -> 0
    | Underlying (_, t) -> t
    | Mul (o1, o2) -> max (getObsMaturityDate o1) (getObsMaturityDate o2)
    | Add (o1, o2) -> max (getObsMaturityDate o1) (getObsMaturityDate o2)
    | Sub (o1, o2) -> max (getObsMaturityDate o1) (getObsMaturityDate o2)
    | Max (o1, o2) -> max (getObsMaturityDate o1) (getObsMaturityDate o2)

/// <summary>
/// Calculates the maturity date of a given contract.
/// </summary>
/// <param name="c">The contract to calculate the maturity date for.</param>
/// <returns>The maturity date of the contract.</returns>
let rec getMaturityDate (c : Contract) : int =
    match c with
    | One _ -> 0
    | Scale (obs, c1) -> max (getMaturityDate c1) (getObsMaturityDate obs)
    | All [] -> 0
    | All (c1::cs) -> max (getMaturityDate c1) (getMaturityDate (All cs))
    | Acquire (i, c1) -> max i (getMaturityDate c1)
    | Or (c1, c2) -> max (getMaturityDate c1) (getMaturityDate c2)
    | Give c1 -> getMaturityDate c1
    | Then (c1, c2) -> max (getMaturityDate c1) (getMaturityDate c2)

/// <summary>
/// Retrieves a list of stocks used in a given contract.
/// </summary>
/// <param name="c">The contract to retrieve the list of stocks for.</param>
/// <returns>The names of stocks used in the contract as a list of strings.</returns>
let rec getStocks (c: Contract) : string list =
    match c with
    | One _ -> []
    | Scale (obs, c') ->
        let stocks_in_obs = get_stocks_from_obs obs
        let stocks_in_c = getStocks c'
        List.append stocks_in_obs stocks_in_c |> List.distinct |> List.sort
    | All cs -> List.map getStocks cs |> List.concat |> List.distinct |> List.sort
    | Acquire (_, c') -> getStocks c'
    | Give c' -> getStocks c'
    | Or (c1, c2) ->
        let stocks_in_c1 = getStocks c1
        let stocks_in_c2 = getStocks c2
        List.append stocks_in_c1 stocks_in_c2 |> List.distinct |> List.sort
    | Then (c1, c2) ->
        let stocks_in_c1 = getStocks c1
        let stocks_in_c2 = getStocks c2
        List.append stocks_in_c1 stocks_in_c2 |> List.distinct |> List.sort

/// <summary>
/// Retrieves a list of stocks used in a given observable value.
/// </summary>
/// <param name="obs">The observable value to retrieve the list of stocks for.</param>
/// <returns>The names of stocks used in the observable as a list of strings.</returns>
and get_stocks_from_obs (obs: Obs) : string list =
    match obs with
    | Value _ -> []
    | Underlying (stock_name, _) -> [stock_name]
    | Mul (obs1, obs2) ->
        let stocks_in_obs1 = get_stocks_from_obs obs1
        let stocks_in_obs2 = get_stocks_from_obs obs2
        List.append stocks_in_obs1 stocks_in_obs2 |> List.distinct |> List.sort
    | Add (obs1, obs2) ->
        let stocks_in_obs1 = get_stocks_from_obs obs1
        let stocks_in_obs2 = get_stocks_from_obs obs2
        List.append stocks_in_obs1 stocks_in_obs2 |> List.distinct |> List.sort
    | Sub (obs1, obs2) ->
        let stocks_in_obs1 = get_stocks_from_obs obs1
        let stocks_in_obs2 = get_stocks_from_obs obs2
        List.append stocks_in_obs1 stocks_in_obs2 |> List.distinct |> List.sort
    | Max (obs1, obs2) ->
        let stocks_in_obs1 = get_stocks_from_obs obs1
        let stocks_in_obs2 = get_stocks_from_obs obs2
        List.append stocks_in_obs1 stocks_in_obs2 |> List.distinct |> List.sort


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
/// Finds the name and date of an underlying.
/// </summary>
/// <param name="obs">The underlying to retrieve the name and date of..</param>
/// <returns>The underlying stock as a observable.</returns>
let getUnderlyingInfo(o : Obs) : string * int =
    match o with
    | Underlying (name, t) -> (name, getObsMaturityDate o)
    | _ -> failwith "not an underlying"
