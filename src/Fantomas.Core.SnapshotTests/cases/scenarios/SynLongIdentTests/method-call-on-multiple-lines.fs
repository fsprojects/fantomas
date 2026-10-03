module Program

[<EntryPoint>]
let main _ =
    try
        try
            Config.Logger.configure ()

            let config =
                ConfigurationBuilder()
                    .SetBasePath(Directory.GetCurrentDirectory())
                    .Build()

            WebHostBuilder()
                .UseConfiguration(config)
                .UseKestrel()
                .UseSerilog()
                .ConfigureAppConfiguration(Action<WebHostBuilderContext, IConfigurationBuilder> configureAppConfiguration)
                .ConfigureServices(Action<WebHostBuilderContext, IServiceCollection> configureServices)
                .Configure(Action<IApplicationBuilder> configureApp)
                .Build()
                .Run()
                |> ignore

            0
        with
        | ex ->
            Log.Fatal(ex, "Service terminated unexpectedly")

            1
    finally
        Log.CloseAndFlush()
