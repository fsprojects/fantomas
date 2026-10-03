type ILModuleReader =
    abstract ILModuleDef: ILModuleDef
    abstract ILAssemblyRefs: ILAssemblyRef list

    // ILModuleReader objects only need to be explicitly disposed if memory mapping is used, i.e. reduceMemoryUsage = false
    inherit IDisposable
