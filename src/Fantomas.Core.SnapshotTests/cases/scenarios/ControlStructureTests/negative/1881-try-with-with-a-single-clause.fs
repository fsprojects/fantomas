// OK
try
    persistState currentState
with ex ->
    printfn "Something went wrong: %A" ex

// OK
try
    persistState currentState
with :? System.ApplicationException as ex ->
    printfn "Something went wrong: %A" ex
