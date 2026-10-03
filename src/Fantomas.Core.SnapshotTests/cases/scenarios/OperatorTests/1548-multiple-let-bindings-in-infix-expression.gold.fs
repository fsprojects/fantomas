/// Used to hide/filter members from base classes based on signature
let MethInfosEquivByNameAndSig erasureFlag ignoreFinal g amap m minfo minfo2 =
    MethInfosEquivByNameAndPartialSig erasureFlag ignoreFinal g amap m minfo minfo2
    && let (CompiledSig(_, retTy, formalMethTypars, _)) = CompiledSigOfMeth g amap m minfo in

       let (CompiledSig(_, retTy2, formalMethTypars2, _)) =
           CompiledSigOfMeth g amap m minfo2 in

       match retTy, retTy2 with
       | None, None -> true
       | Some retTy, Some retTy2 ->
           typeAEquivAux erasureFlag g (TypeEquivEnv.FromEquivTypars formalMethTypars formalMethTypars2) retTy retTy2
       | _ -> false
