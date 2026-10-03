(*---
fsharp_max_infix_operator_expression = 75
---*)
    do
#if FOOBAR

#endif
        assembly.MainModule.Attributes <- assembly.MainModule.Attributes &&& (~~~ModuleAttributes.StrongNameSigned)
        assemblyName.HasPublicKey <- false
        assemblyName.PublicKey <- null
        assemblyName.PublicKeyToken <- null
