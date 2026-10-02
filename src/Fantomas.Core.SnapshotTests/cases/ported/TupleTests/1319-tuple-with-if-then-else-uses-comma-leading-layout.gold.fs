let y =
    if String.IsNullOrWhiteSpace(args) then
        ""
    elif args.StartsWith("(") then
        args
    elif
        v.CurriedParameterGroups.Count > 1
        && (not verboseMode)
    then
        " " + args
    else
        sprintf "(%s)" args
    , namesWithIndices
