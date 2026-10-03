CloudStorageAccount.SetConfigurationSettingPublisher(fun configName configSettingPublisher ->
    let connectionString =
        if hostedService then
            RoleEnvironment.GetConfigurationSettingValue(configName)
        else
            ConfigurationManager.ConnectionStrings.[configName].ConnectionString

    configSettingPublisher.Invoke(connectionString)
    |> ignore)
