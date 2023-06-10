module XMLFunctions
open Domain
open System.Xml.Linq
open System.Linq
open System.IO

/// <summary>
/// Represents a stock, which has a name, price, drift, volatility, and a list of prices at different times.
/// </summary>
type Stock = 
    | Name of string
    | Price of float
    | Drift of float
    | Volatility of float
    | Prices of (int * float) list


let XMLfilePath = __SOURCE_DIRECTORY__ + "/../src/stock_data.xml"
let stockData = XDocument.Load(XMLfilePath)
let xn = XName.Get


/// <summary>
/// Retrieves the current price of a given stock symbol.
/// </summary>
/// <param name="symbol">The symbol of the stock to retrieve the price for.</param>
/// <returns>A Result object containing the stock name and current price if successful, otherwise an error message.</returns>
let getCurrentStockPrice (symbol: string) : Result<float, string> =
    let stockData = XDocument.Load(XMLfilePath)
    match stockData.Descendants(XName.Get("stock")).SingleOrDefault(fun s -> s.Element(XName.Get("Name")).Value = symbol) with
    | null -> Error($"Stock symbol '{symbol}' not found.")
    | stock ->
        match stock.Descendants(XName.Get("Price")).SingleOrDefault(fun e -> (e : XElement).Attribute(XName.Get("index")).Value = "0") with
        | null -> Error($"Current price for stock symbol '{symbol}' not found.")
        | priceElem -> Ok (float priceElem.Value)

/// <summary>
/// Retrieves the parameters current price, drift and volatility for a given stock.
/// </summary>
/// <param name="stockName">The name of the stock to retrieve the parameters for.</param>
/// <returns>A tuple containing the current price, drift, and volatility for the given stock, if available; otherwise, None.</returns>
let getStockParameters (stockName: string) : (float * float * float) option =
    try
        let stock = stockData.Descendants(xn "stock").SingleOrDefault(fun s -> s.Element(xn "Name").Value = stockName)
        match stock with
        | null -> None
        | _ ->
            match getCurrentStockPrice(stockName) with
            | Ok price ->
                let drift = stock.Element(xn "Drift").Value |> float
                let volatility = stock.Element(xn "Volatility").Value |> float
                Some (price, drift, volatility)
            | Error _ -> None
    with
    | _ -> None


/// <summary>
/// Returns the price of a given stock at a given time (if available).
/// </summary>
/// <param name="stockName">The name of the stock.</param>
/// <param name="time">The time at which to retrieve the price.</param>
/// <returns>The price of the stock at the given time, if available; otherwise, None.</returns>
let getPrice (stockName: string) (time: int) : float =
    match stockData.Descendants(xn "stock")
                    .SingleOrDefault(fun s -> s.Element(xn "Name").Value = stockName) with
    | null -> failwith "Stock not found"
    | stock ->
        match stock.Descendants(xn "Price")
                    .SingleOrDefault(fun p -> int (p.Attribute(xn "index")).Value = time) with
        | null -> failwith "Price not found"
        | price -> float(price.Value)


(* The rest of the functions in this file are not used in the thesis *)


/// <summary>parses stock data from an XML document and creates a list of stock objects.</summary>
/// <returns> a list of stock objects.</returns>
let stockPrices = 
    stockData.Descendants(xn "stock")
    |> Seq.map(fun s -> 
        let name = s.Descendants(xn "Name") |> Seq.head |> fun e -> e.Value
        let price = s.Descendants(xn "Price") |> Seq.head |> fun e -> float e.Value
        let drift = s.Descendants(xn "Drift") |> Seq.head |> fun e -> float e.Value
        let volatility = s.Descendants(xn "Volatility") |> Seq.head |> fun e -> float e.Value
        let prices = 
            s.Descendants(xn "Price")
            |> Seq.filter (fun e -> e.Attribute(xn "index") <> null)
            |> Seq.map(fun e -> 
                let index = e.Attribute(xn "index").Value |> int
                let value = e.Value |> float
                (index, value)
            )
            |> List.ofSeq
        Name name, Price price, Drift drift, Volatility volatility, Prices prices
    )
    |> List.ofSeq



/// <summary>
/// Updates the price data for a given stock at a given time.
/// </summary>
/// <param name="stockName">The name of the stock to update.</param>
/// <param name="time">The time at which to update the price.</param>
/// <param name="price">The new price value.</param>
/// <returns>Void.</returns>
let updateStockData (stockName: string) (time: int) (price: float) : unit =
    let stock = stockData.Descendants(xn "stock")
                  .Single(fun e -> e.Element(xn "Name").Value = stockName)
    let prices = stock.Element(xn "Prices").Descendants(xn "Price")

    let existingPrice (prices : seq<XElement>) (time : int) : bool =
        match Seq.tryFind (fun e -> (e : XElement).Attribute(xn "index").Value = time.ToString()) prices with
        | Some(_) -> true
        | None -> false

    // update or add price

    match existingPrice prices time with
    | true -> 
        let priceElem = prices |> Seq.find (fun e -> e.Attribute(xn "index").Value = time.ToString())
        priceElem.Value <- price.ToString()
    | false ->
        let prices = stock.Element(xn "Prices")
        prices.Add(new XElement(xn "Price", price, new XAttribute(xn "index", time)))

    // write changes to file
    use stream = new FileStream(XMLfilePath, FileMode.Create, FileAccess.Write, FileShare.None)
    stockData.Save(stream)

    printfn "Changes saved to file."


