module Infrastructure =

    let internal ReportMessage
        (message: string)
        #if DEBUG
        #else
        (errorLevel: ErrorLevel)
        #endif
        =
        #if DEBUG
        #else
        let sentryEvent = SentryEvent(SentryMessage message, Level = errorLevel)
        ()
#endif
