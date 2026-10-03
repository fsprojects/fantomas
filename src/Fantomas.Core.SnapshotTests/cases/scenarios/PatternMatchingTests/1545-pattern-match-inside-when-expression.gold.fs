let GenApp (cenv: cenv) cgbuf eenv (f, fty, tyargs, curriedArgs, m) sequel =
    let g = cenv.g

    match (f, tyargs, curriedArgs) with
    // Look for tailcall to turn into branch
    | (Expr.Val(v, _, _), _, _) when
        match ListAssoc.tryFind g.valRefEq v eenv.innerVals with
        | Some(kind, _) ->
            (not v.IsConstructor
             &&
             // when branch-calling methods we must have the right type parameters
             (match kind with
              | BranchCallClosure _ -> true
              | BranchCallMethod(_, _, tps, _, _, _) ->
                  (List.lengthsEqAndForall2 (fun ty tp -> typeEquiv g ty (mkTyparTy tp)) tyargs tps))
             &&
             // must be exact #args, ignoring tupling - we untuple if needed below
             (let arityInfo =
                 match kind with
                 | BranchCallClosure arityInfo
                 | BranchCallMethod(arityInfo, _, _, _, _, _) -> arityInfo

              arityInfo.Length = curriedArgs.Length)
             &&
             (* no tailcall out of exception handler, etc. *)
             (match sequelIgnoringEndScopesAndDiscard sequel with
              | Return
              | ReturnVoid -> true
              | _ -> false))
        | None -> false
        ->
        let (kind, mark) = ListAssoc.find g.valRefEq v eenv.innerVals // already checked above in when guard
        ()
