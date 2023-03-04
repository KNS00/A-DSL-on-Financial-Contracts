module Program
open DSL
open Analysis
open Tests
open XPlot.Plotly

let startTime = 0.0
let endTime = 10.0
let dt = 0.01
let sigma = 0.1
let rs = [0.8; 1.0; 1.2; 1.4; 1.6; 1.8]

let data = 
    rs
    |> List.map (fun r ->
        let gbm = GBM startTime endTime dt r sigma
        Scatter(
            x = seq { startTime .. dt .. endTime }, 
            y = gbm, 
            mode = "lines", 
            name = sprintf "GBM (r = %.1f)" r
        )
    )

let layout = Layout(title = "Geometric Brownian Motion")
Chart.Plot(data)
|> Chart.Show 

let main args =
    0

printer |> ignore