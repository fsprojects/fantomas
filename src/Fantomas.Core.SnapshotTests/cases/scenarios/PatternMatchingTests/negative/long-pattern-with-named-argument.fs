let addSpaceBeforeParensInFunDef (spaceBeforeSetting: bool) (functionOrMethod: SynLongIdent) args =
    match functionOrMethod, args with
    | SynLongIdent(id = [ newIdent ]), _ when newIdent.idText = "new" -> false
    | _ -> true
