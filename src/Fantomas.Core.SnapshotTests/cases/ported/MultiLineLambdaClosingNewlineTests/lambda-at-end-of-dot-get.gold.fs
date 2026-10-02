configuration.MinimumLevel
    .Debug()
    .WriteTo.Logger(fun loggerConfiguration ->
        loggerConfiguration.Enrich
            .WithProperty("host", Environment.MachineName)
            .Enrich.WithProperty("user", Environment.UserName)
            .Enrich.WithProperty("application", context.HostingEnvironment.ApplicationName)
        |> ignore
    )
