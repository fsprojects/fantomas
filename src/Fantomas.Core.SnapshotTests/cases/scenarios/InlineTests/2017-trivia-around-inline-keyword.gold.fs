let
#if !DEBUG
    inline
#endif
    map
        f
        ar
        =
    Async.map (Result.map f) ar
