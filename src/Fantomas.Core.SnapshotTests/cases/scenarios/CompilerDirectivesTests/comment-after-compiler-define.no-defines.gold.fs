#if EXTENDED_EXTENSION_MEMBERS // indicates if extension members can add additional constraints to type parameters
#else
let tcrefObjTy, enclosingDeclaredTypars, renaming, objTy =
    FreshenTyconRef m rigid tcref declaredTyconTypars
#endif
