serviceCollection
    .AddSingleton<IClock>(systemClock)
    .AddOptions<MyOptions>(configureOptions)
