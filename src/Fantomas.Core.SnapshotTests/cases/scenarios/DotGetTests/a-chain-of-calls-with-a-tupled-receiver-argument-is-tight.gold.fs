Log.Logger <-
    LoggerConfiguration(1, 2)
        .Destructure.FSharpTypes()
        .WriteTo.Console()
        .CreateLogger()
