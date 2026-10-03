configuration.MinimumLevel
    .Debug()
    .WriteTo.Logger(fun x -> x * x)
