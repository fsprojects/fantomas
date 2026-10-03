// Link all the assemblies together and produce the input typecheck accumulator
let CombineImportedAssembliesTask
    (
        a,
        b
#if !NO_TYPEPROVIDERS
        , c
#endif
    ) =

        ()
