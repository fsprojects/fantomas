let x =
    builder
        .Configure(
#if DEBUG
            debugOptions
#else
            releaseOptions
#endif
        )
        .Build()
        .Result
