let x =
    LoggerConfiguration<Foo>()
        .Enrich.WithProperty<Bar>("user", Environment.UserName)
        .Enrich.WithProperty("application", context.HostingEnvironment.ApplicationName)
