fun config ->
    #if LOGGING_DEBUG || LOGGING_LOCAL
    #endif

    config
#if LOGGING_DEBUG
#endif
#if LOGGING_LOCAL
#endif
