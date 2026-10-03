[<Literal>]
let private assemblyConfig =
    #if DEBUG
    #if TRACE
    "DEBUG;TRACE"
#else
#endif
#else
#if TRACE
#else
#endif
#endif
