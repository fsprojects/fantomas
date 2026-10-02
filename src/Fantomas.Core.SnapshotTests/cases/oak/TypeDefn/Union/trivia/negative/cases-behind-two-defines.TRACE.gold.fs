type Channel =
    | Stable
    #if DEBUG
    #endif
    #if TRACE
    | Trace
#endif
