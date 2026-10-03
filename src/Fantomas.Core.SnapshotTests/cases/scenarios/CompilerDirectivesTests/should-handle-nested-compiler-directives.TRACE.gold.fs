[<Literal>]
let private assemblyConfig =
    #if DEBUG
    #if TRACE
    #else
    #endif
    #else
    #if TRACE
    "TRACE"
#else
#endif
#endif
