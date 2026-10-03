let a =
    // some comment
    42
let b (x:int) (y:int):int =
    printfn "doing b with %i %i" x y
    x + y
let c () =
    try
        0
    with ex -> 1
