let x =
    not (isObjTy g ty)
    && isAppTy g ty
    && isObjTy g minfo.ApparentEnclosingType
    && let tcref = tcrefOfAppTy g ty in
       match tcref.TypeReprInfo with
       | TProvidedTypeExtensionPoint info ->
           info.ProvidedType.PUntaint(
               (fun st ->
                   (st :> IProvidedCustomAttributeProvider)
                       .GetHasTypeProviderEditorHideMethodsAttribute(info.ProvidedType.TypeProvider.PUntaintNoFailure(id))),
               m
           )
       | _ ->
           if tcref.IsILTycon then
               tcref.ILTyconRawMetadata.CustomAttrs.AsArray
               |> Array.exists (fun attr ->
                   attr.Method.DeclaringType.TypeSpec.Name = typeof<TypeProviderEditorHideMethodsAttribute>.FullName)
           else
               false
