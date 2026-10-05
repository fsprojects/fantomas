let private knownProviders =
    [
#if !FABLE_COMPILER
        (SerilogProvider.isAvailable, SerilogProvider.create)
        (MicrosoftExtensionsLoggingProvider.isAvailable, MicrosoftExtensionsLoggingProvider.create)
#endif
    ]
