type Foobar =
    member tcConfig.IsSystemAssembly (filename: string) =
        try
            FileSystem.SafeExists filename &&
            ((tcConfig.GetTargetFrameworkDirectories() |> List.exists (fun clrRoot -> clrRoot = Path.GetDirectoryName filename)) ||
             (tcConfig.FxResolver.GetSystemAssemblies().Contains (fileNameWithoutExtension filename)) ||
             tcConfig.FxResolver.IsInReferenceAssemblyPackDirectory filename)
        with _ ->
            false
