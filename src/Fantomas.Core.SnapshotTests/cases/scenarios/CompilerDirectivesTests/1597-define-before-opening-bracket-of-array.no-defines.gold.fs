let Environment =
    { new IEnvironment with
        member _.IsWindows() =
            InteropServices.RuntimeInformation.IsOSPlatform(InteropServices.OSPlatform.Windows)

        member _.GetScriptArgs() =
            #if INTERACTIVE
            #else
            [||]
        #endif

        member _.GetEnvironmentVariable(varName) =
            System.Environment.GetEnvironmentVariable(varName)

        member _.SetEnvironmentVariable(varName, value) =
            System.Environment.SetEnvironmentVariable(varName, value) }
