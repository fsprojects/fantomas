let internal UpdateStrongNaming (assembly: AssemblyDefinition) (key: StrongNameKeyPair option) =
    let assemblyName = assembly.Name
    #if NETCOREAPP2_0
    #else
    match key with
    | None ->
        #endif
        assembly.MainModule.Attributes <- assembly.MainModule.Attributes &&& (~~~ModuleAttributes.StrongNameSigned)
        assemblyName.HasPublicKey <- false
        assemblyName.PublicKey <- null
        assemblyName.PublicKeyToken <- null
    #if NETCOREAPP2_0
    #else
    | Some key' ->
        assemblyName.HasPublicKey <- true
        assemblyName.PublicKey <- key'.PublicKey // sets token implicitly
#endif
