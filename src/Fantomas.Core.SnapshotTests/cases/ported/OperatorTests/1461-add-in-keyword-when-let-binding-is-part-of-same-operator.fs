    let isUnseenByHidingAttribute () =
        not (isObjTy g ty) &&
        isAppTy g ty &&
        isObjTy g minfo.ApparentEnclosingType &&
        let tcref = tcrefOfAppTy g ty
        match tcref.TypeReprInfo with
        | _ -> false
