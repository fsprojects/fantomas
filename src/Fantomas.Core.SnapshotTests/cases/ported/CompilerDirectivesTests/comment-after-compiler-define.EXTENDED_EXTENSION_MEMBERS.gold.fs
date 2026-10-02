#if EXTENDED_EXTENSION_MEMBERS // indicates if extension members can add additional constraints to type parameters
let tcrefObjTy, enclosingDeclaredTypars, renaming, objTy =
    FreshenTyconRef
        m
        (if isExtrinsic then
             TyparRigidity.Flexible
         else
             rigid)
        tcref
        declaredTyconTypars
#else
#endif
