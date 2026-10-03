[<Literal>]
let private assemblyConfig =
#if DEBUG
    ()
#else
#if TRACE
    "TRACE"
#else
    ""
#endif
#endif
