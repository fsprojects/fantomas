let foo bar =
    let tfms =
        [
            #if NET6_0_OR_GREATER
            "net6.0"
            #endif
            #if NET7_0_OR_GREATER
            "net7.0"
        #endif
        ]

    ()
