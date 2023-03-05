module Program
open DSL
open Analysis
open Tests

let main args =
    plotProcess [wp1; gbm1]

main [] |> ignore