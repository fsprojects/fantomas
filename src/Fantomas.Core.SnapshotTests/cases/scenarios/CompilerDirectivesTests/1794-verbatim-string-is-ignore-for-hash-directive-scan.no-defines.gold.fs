let ProgramFilesX86 =
    match wow64, globalArch with
    | "AMD64", "AMD64"
    | null, "AMD64"
    | "x86", "AMD64" -> Environment.GetEnvironmentVariable "ProgramFiles(x86)"
    | _ -> Environment.GetEnvironmentVariable "ProgramFiles"
    |> fun detected ->
        if detected = null then
            @"C:\Program Files (x86)\"
        else
            detected

let isUnix =
    #if NETSTANDARD1_6 || NETSTANDARD2_0
    #else
    int Environment.OSVersion.Platform
    |> fun p -> (p = 4) || (p = 6) || (p = 128)
#endif
