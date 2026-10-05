builder
    .Connect(hostName)
    .Configuration.Database
    .PrimaryConnection.Settings.Apply(spec)
    .Build()
