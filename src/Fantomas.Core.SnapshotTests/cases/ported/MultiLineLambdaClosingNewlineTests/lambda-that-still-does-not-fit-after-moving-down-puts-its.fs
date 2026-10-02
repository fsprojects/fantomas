(*---
max_line_length = 70
fsharp_multi_line_lambda_closing_newline = true
---*)
let dotted () =
    storage.SetConfigurationSettingPublisher(fun configName publisher -> publishTheConfigurationValue configName publisher andThenSomethingElse)

let undotted () =
    storageSetConfigurationSettingPublisher (fun configName publisher -> publishTheConfigurationValue configName publisher andThenSomethingElse)
