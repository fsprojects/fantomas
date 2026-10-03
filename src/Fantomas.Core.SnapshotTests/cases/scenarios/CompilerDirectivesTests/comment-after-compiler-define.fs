(*---
fsharp_max_if_then_else_short_width = 20
---*)
#if EXTENDED_EXTENSION_MEMBERS // indicates if extension members can add additional constraints to type parameters
    let tcrefObjTy, enclosingDeclaredTypars, renaming, objTy = FreshenTyconRef m (if isExtrinsic then TyparRigidity.Flexible else rigid) tcref declaredTyconTypars
#else
    let tcrefObjTy, enclosingDeclaredTypars, renaming, objTy = FreshenTyconRef m rigid tcref declaredTyconTypars
#endif
