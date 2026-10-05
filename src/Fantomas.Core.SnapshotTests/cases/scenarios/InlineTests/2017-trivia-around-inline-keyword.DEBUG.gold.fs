let
    #if !DEBUG
    #endif
    map
        f
        ar
        =
    Async.map (Result.map f) ar
