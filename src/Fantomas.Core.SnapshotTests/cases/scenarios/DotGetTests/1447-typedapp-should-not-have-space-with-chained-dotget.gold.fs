let x =
    LoggerConfiguration<Foo>("host", Environment.MachineName)
        .Enrich.WithProperty<Bar>("user", Environment.UserName)
        .Enrich.WithProperty("application", context.HostingEnvironment.ApplicationName)
