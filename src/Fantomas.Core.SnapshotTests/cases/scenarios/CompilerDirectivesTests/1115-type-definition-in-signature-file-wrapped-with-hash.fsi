namespace X

type UnresolvedAssemblyReference = UnresolvedAssemblyReference of string * AssemblyReference list

#if !NO_EXTENSIONTYPING
type ResolvedExtensionReference = ResolvedExtensionReference of string * AssemblyReference list * Tainted<ITypeProvider> list
#endif
