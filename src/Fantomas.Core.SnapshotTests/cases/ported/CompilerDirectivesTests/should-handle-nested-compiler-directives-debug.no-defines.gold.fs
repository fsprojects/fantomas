[<Literal>]
let private assemblyConfig =
    #if DEBUG
    #else
    #if TRACE
    #else
    ""
#endif
#endif
