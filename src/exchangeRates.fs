module exchangeRates
open System.Net
open System.IO
open System.Text.Json

// A type for the exchange rate JSON response
type ExchangeRateResponse = {
    [<Literal>] ``base``: string
    rates: Map<string, float>
}

// Make a GET request to the API 
let url = "https://cdn.forexvalutaomregner.dk/api/nationalbanken.json"
let request = WebRequest.Create(url)
let response = request.GetResponse()
use stream = response.GetResponseStream()
use reader = new StreamReader(stream)
let json = reader.ReadToEnd()
let exchangeRates = JsonSerializer.Deserialize<ExchangeRateResponse>(json)

// Getting exchange rates from DKK, GBP, and EUR to USD
let getDKKUSD() : float = exchangeRates.rates.["USD"] / exchangeRates.rates.["DKK"]

let getGBPUSD() : float = exchangeRates.rates.["USD"] / exchangeRates.rates.["GBP"]

let getEURUSD() : float = exchangeRates.rates.["USD"] / exchangeRates.rates.["EUR"]
