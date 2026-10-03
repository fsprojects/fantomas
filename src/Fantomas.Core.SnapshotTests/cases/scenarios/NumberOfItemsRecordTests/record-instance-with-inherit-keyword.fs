(*---
fsharp_record_multiline_formatter = number_of_items
fsharp_multiline_bracket_style = cramped
---*)
let a =
        { inherit ProjectPropertiesBase<_>(projectTypeGuids, factoryGuid, targetFrameworkIds, dotNetCoreSDK)
          buildSettings = FSharpBuildSettings()
          targetPlatformData = targetPlatformData }
