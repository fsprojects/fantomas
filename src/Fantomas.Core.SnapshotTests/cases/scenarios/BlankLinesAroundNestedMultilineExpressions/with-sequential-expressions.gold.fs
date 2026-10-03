let topLevelFunction () =
    printfn "Something to print"
    try
        nothing ()
    with ex ->
        splash ()
    ()
