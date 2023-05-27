module Management
open Domain
open XMLFunctions


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
    | Scale (obs, c1) -> max (maturity c1) (obsMaturity obs)
    | All [] -> 0
    | All (c1::cs) -> max (maturity c1) (maturity (All cs))
    | Acquire (_, t, c1) -> max t (maturity c1)
    | Or (c1, c2) -> max (maturity c1) (maturity c2)
    | Give c1 -> maturity c1
    | Then (c1, c2) -> max (maturity c1) (maturity c2)


