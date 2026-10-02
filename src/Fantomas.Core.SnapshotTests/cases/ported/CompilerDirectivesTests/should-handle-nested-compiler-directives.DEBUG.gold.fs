[<Literal>]
let private assemblyConfig =
    #if DEBUG
    #if TRACE
    #else
    "DEBUG"
#endif
#else
#if TRACE
#else
#endif
#endif
