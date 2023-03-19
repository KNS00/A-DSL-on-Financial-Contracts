module Program
open DSL
open Analysis
open Tests

let main args =
    printfn "%s %f" "Test E function:" TestStockPrice 
    printfn "%s %A" "Test Evaluation of contract1:" result1 // 1.0,     correct
    printfn "%s %A" "Test Evaluation of contract2:" result2 // 100.0,   correct
    printfn "%s %A" "Test Evaluation of contract3:" result3 // 9.99,   correct
    printfn "%s %A" "Test Evaluation of contract4:" result4 // 98.02,  correct
    printfn "%s %A" "Test Evaluation of contract5:" result5 // 209.02,   correct
    printfn "%s %A" "Test Evaluation of call option:" result6 // Should be around 115 beacuse E[S_T] = 165 and 165 - 50 = 115. However, this will
                                                          // not work for exotic options! Because E[F(S_T)] != F(E[S_T]) for non-linear functions!
main [] 