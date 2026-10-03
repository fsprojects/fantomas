let x =
    builder
        .Configure(
            #if DEBUG
            debugOptions
        #else
        #endif
        )
        .Build()
        .Result
