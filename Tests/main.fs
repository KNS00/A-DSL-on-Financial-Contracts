open Domain
open XMLFunctions
open Evaluations
open Simulations
open Analysis
open Examples
open Management
open ManagementTests
open Program
open simulationTests
open FsUnit
open Xunit

module testProgram =
    let squared(x : float) 
        = x**2.0
    [<Theory>]
    [<InlineData(1.0, 1.0)>]
    [<InlineData(5.0, 25)>]
    [<InlineData(-5.7, 32.49)>]
    let ``test square``(input : float) (expectedOutput : float) : unit =
        squared(input) |> should (equalWithin 1e-7) expectedOutput

    let [<EntryPoint>] main _ =
        0