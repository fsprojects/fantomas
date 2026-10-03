let x =
    builder
        .Configure(
            #if DEBUG
            #else
            releaseOptions
        #endif
        )
        .Build()
        .Result
