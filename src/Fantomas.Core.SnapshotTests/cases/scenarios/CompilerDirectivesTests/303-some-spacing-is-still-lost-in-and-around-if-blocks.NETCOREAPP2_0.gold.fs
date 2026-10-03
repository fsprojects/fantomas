let internal UpdateStrongNaming (assembly: AssemblyDefinition) (key: StrongNameKeyPair option) =
    let assemblyName = assembly.Name
    #if NETCOREAPP2_0
    do
        #else
        #endif
        assembly.MainModule.Attributes <- assembly.MainModule.Attributes &&& (~~~ModuleAttributes.StrongNameSigned)
        assemblyName.HasPublicKey <- false
        assemblyName.PublicKey <- null
        assemblyName.PublicKeyToken <- null
#if NETCOREAPP2_0
#else
#endif
