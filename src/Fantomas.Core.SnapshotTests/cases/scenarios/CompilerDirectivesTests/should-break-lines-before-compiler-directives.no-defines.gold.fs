[<Literal>]
let private assemblyConfig () =
    #if TRACE
    #else
    let x = "x"
    #endif
    x
