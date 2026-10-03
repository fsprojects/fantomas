(*---
fsharp_experimental_keep_indent_in_branch = true
---*)
let mapOperationToWebPart (operation: OpenApiOperationDescription) : SynExpr =
    let verb = mkIdentExpr (operation.Method.ToUpper())
    let route =
        match operation with
        | _ -> // no route parameters
        let route = mkAppNonAtomicExpr (mkIdentExpr "route") (mkStringExprConst operation.Path)
        let responseHttpFunc =
            mkLambdaExpr [ ] unitExpr
            |> mkParenExpr
        infixFish route responseHttpFunc

    infixFish verb route
