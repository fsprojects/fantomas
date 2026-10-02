(*---
max_line_length = 70
---*)
let dotted () =
    storage.SetConfigurationSettingPublisher(fun configName publisher -> publishTheConfigurationValue configName publisher andThenSomethingElse)

let undotted () =
    storageSetConfigurationSettingPublisher (fun configName publisher -> publishTheConfigurationValue configName publisher andThenSomethingElse)
