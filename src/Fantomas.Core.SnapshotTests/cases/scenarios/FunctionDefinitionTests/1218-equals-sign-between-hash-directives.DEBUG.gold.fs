module Infrastructure =

    let internal ReportMessage
        (message: string)
        #if DEBUG
        (_: ErrorLevel)
        #else
        #endif
        =
        #if DEBUG
        failwith message
#else
#endif
