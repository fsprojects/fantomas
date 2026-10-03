do
    #if FOOBAR

    #endif
    assembly.MainModule.Attributes <- assembly.MainModule.Attributes &&& (~~~ModuleAttributes.StrongNameSigned)
    assemblyName.HasPublicKey <- false
    assemblyName.PublicKey <- null
    assemblyName.PublicKeyToken <- null
