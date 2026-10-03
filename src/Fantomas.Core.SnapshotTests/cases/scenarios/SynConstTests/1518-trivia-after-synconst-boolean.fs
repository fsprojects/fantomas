    match ast with
    | ParsedInput.SigFile _input ->
        // There is not much to explore in signature files
        true
    | ParsedInput.ImplFile input -> validateImplFileInput input

    match t with
    | TTuple _ -> not node.IsEmpty
    | TFun _ -> true // Fun is grouped by brackets inside 'genType astContext true t'
    | _ -> false

    let condition e =
        match e with
        | ElIf _
        | SynExpr.Lambda _ -> true
        | _ -> false // "if .. then .. else" have precedence over ","

    let x = 9
