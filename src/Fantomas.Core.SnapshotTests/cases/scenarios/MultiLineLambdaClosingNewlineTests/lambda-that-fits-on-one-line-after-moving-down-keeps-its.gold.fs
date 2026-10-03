let dotted () =
    storage.SetConfigurationSettingPublisher
        (fun configName publisher -> publish configName publisher)

let undotted () =
    storageSetConfigurationSettingPublisher
        (fun configName publisher -> publish configName publisher)
