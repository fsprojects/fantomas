let start (args: IArgs) =
    // Serilog configuration
    Log.Logger <-
        LoggerConfiguration()
            .MinimumLevel.Debug()
            .MinimumLevel.Override("Microsoft", LogEventLevel.Information)
            .Enrich.FromLogContext()
            .WriteTo.Console()
            .WriteTo.File(Path.Combine(args.ContentRoot, "temp/log.txt"))
            .CreateLogger()

    try
        try
            let giraffeApp = configureGiraffeApp args

            WebHost
                .CreateDefaultBuilder()
                .UseWebRoot(args.ClientPath)
                #if DEBUG
                .UseContentRoot(args.ContentRoot)
                .UseUrls(args.Host + ":" + string args.Port)
                #endif
                .UseSerilog()
                .Configure(Action<IApplicationBuilder>(configureApp giraffeApp))
                .ConfigureServices(configureServices args)
                .Build()
                .Run()

            0
        with ex ->
            Log.Fatal(ex, "Host terminated unexpectedly")
            1
    finally
        Log.CloseAndFlush()
