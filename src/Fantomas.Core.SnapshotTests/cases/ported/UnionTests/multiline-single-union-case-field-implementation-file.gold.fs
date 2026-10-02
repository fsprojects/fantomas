namespace X

type UnresolvedAssemblyReference = UnresolvedAssemblyReference of string * AssemblyReference list

type ResolvedExtensionReference =
    | ResolvedExtensionReference of string * AssemblyReference list * Tainted<ITypeProvider> list
