module Analysis
open Domain
open Evaluations
open Simulations
//open exchangeRates

/// <summary>
/// Runs a Monte Carlo simulation to estimate the price of a portfolio of stocks at a given time. Also updates the XML file with these prices.
/// </summary>
/// <param name="stockList">A list of stocks to simulate.</param>
/// <returns>A (string * int * float) list, with the name, time and price of the underlying.</returns>
let simulateStocks (stockList : Obs list) : (string * int * float) list =
    let stocksInfo: (string * int) list =
        stockList
        |> List.collect (fun o ->
            let stocks = get_stocks_from_obs o
            let maturityDate = getObsMaturityDate o
            List.zip stocks (List.replicate (List.length stocks) maturityDate))
        |> List.distinct
        |> List.sort

    let dt = 1.0
    let sims = 100_000

    let simulate (stock: string, t: int) : (string * int * float) =
        let rec loop (i: int) (acc : float) : float =
            if i = sims then acc
            else 
                let simulationResult = mc1 [stock] t dt
                let updatedAcc = acc + List.head simulationResult
                loop (i + 1) updatedAcc
        let totalSimulatedPrice = loop 0 0.0
        let averageSimulatedPrice = totalSimulatedPrice / float sims
        (stock, t, averageSimulatedPrice)

    let averagePrices = List.map simulate stocksInfo
    List.iter (fun (stockName, t, avgPrice) -> XMLFunctions.updateStockData stockName t avgPrice) averagePrices
    averagePrices


/// <summary>
/// Runs a Monte Carlo simulation to estimate the expected value of a given contract.
/// Uses the given options underlying stocks to generate stock price before simulation.
/// </summary>
/// <param name="c1">The contract to simulate.</param>
/// <returns>The expected value of the option.</returns>
let simulateContract (c1 : Contract) : float = // The reason why this function has to be so complicated is that we need to simulate sims amount of stock prices first, and then use these for the evaluation of contracts. For each evaluation, we need to use a different stock price, otherwise we just get the same price for each simulation!
    let generateStockPrices (stocksInfo: (string * int) list) (sims: int) : Map<(string * int), float list> =
        stocksInfo
        |> List.map (fun (stock, maturityDate) -> (stock, maturityDate), [for _ in 1..sims -> List.head(mc1 [stock] maturityDate 1.0)])
        |> Map.ofList

    let lookup_stock_prices (stockPrices: Map<(string * int), float list> ref) (stock: string) (maturityDate: int) =
        let key = (stock, maturityDate)
        let prices = Map.find key !stockPrices // mutable so we can remove stockPrices when they have been used in the simulation
        let price = List.head prices
        let updatedPrices = List.tail prices
        stockPrices := Map.add key updatedPrices !stockPrices
        price
    let sims : int = 100_000
    let stockList : Obs List = getStocksAsObs c1
    let stocksInfo : (string * int) list =
        stockList
        |> List.collect (fun o ->
            let stocks = get_stocks_from_obs o
            let maturityDate = getObsMaturityDate o
            List.zip stocks (List.replicate (List.length stocks) maturityDate))
        |> List.distinct
        |> List.sort
    let stockPrices : Map<(string * int),float list> ref = ref (generateStockPrices stocksInfo sims)

    let E (stockAndMaturity: string * int) = lookup_stock_prices stockPrices (fst stockAndMaturity) (snd stockAndMaturity)
    let evaluations : float list = [for _ in 1..sims -> evalc I E c1]
    evaluations
    |> List.average

    