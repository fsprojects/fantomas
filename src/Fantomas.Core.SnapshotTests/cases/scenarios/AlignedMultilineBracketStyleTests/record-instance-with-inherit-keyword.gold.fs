let a =
    {
        inherit ProjectPropertiesBase<_>(projectTypeGuids, factoryGuid, targetFrameworkIds, dotNetCoreSDK)
        buildSettings = FSharpBuildSettings()
        targetPlatformData = targetPlatformData
    }
