let detectZeroOR point =
    match point with
    | (0, 0)
    | (0, _)
    | (_, 0) -> printfn "Zero found."
    | _ -> printfn "Both nonzero."

let detectZeroAND point =
    match point with
    | (0, 0) -> printfn "Both values zero."
    | (var1, var2) & (0, _) -> printfn "First value is 0 in (%d, %d)" var1 var2
    | (var1, var2) & (_, 0) -> printfn "Second value is 0 in (%d, %d)" var1 var2
    | _ -> printfn "Both nonzero."
