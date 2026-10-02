let internal UpdateStrongNaming
    (assembly : AssemblyDefinition)
    (key : StrongNameKeyPair option)
    =
    assembly.Name

let UpdateStrongNamingX (assembly : AssemblyDefinition) (key : StrongNameKeyPair option) =
    assembly.Name
