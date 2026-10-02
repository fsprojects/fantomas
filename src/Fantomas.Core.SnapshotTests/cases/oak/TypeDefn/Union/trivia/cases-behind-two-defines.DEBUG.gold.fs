type Channel =
    | Stable
    #if DEBUG
    | Debug
#endif
#if TRACE
#endif
