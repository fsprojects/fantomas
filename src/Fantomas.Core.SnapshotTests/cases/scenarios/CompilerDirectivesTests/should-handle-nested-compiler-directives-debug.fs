let [<Literal>] private assemblyConfig =
    #if DEBUG
        ()
    #else
        #if TRACE
            "TRACE"
        #else
            ""
        #endif
    #endif
