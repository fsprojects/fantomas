let Environment =
    { new IEnvironment with
        member _.IsWindows() =
            InteropServices.RuntimeInformation.IsOSPlatform(InteropServices.OSPlatform.Windows)

        member _.GetScriptArgs() =
            #if INTERACTIVE
            fsi.CommandLineArgs |> Array.skip 1
        #else
        #endif

        member _.GetEnvironmentVariable(varName) =
            System.Environment.GetEnvironmentVariable(varName)

        member _.SetEnvironmentVariable(varName, value) =
            System.Environment.SetEnvironmentVariable(varName, value) }
