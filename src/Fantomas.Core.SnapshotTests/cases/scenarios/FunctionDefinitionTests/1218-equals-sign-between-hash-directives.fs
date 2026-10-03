module Infrastructure =

    let internal ReportMessage
        (message: string)
#if DEBUG
        (_: ErrorLevel)
#else
        (errorLevel: ErrorLevel)
#endif
        =
#if DEBUG
        failwith message
#else
        let sentryEvent = SentryEvent (SentryMessage message, Level = errorLevel)
        ()
#endif
