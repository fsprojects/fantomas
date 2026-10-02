(*---
fsharp_max_infix_operator_expression = 50
---*)
let argExpr =
    col sepNln es (fun e ->
        let genLambda
            (pats: Context -> Context)
            (bodyExpr: SynExpr)
            (lpr: Range)
            (rpr: Range option)
            (arrowRange: Range)
            (pr: Range)
            : Context -> Context =
            leadingExpressionIsMultiline (sepOpenTFor lpr -- "fun "
                                            +> pats
                                            +> genArrowWithTrivia
                                                (genExprKeepIndentInBranch astContext bodyExpr)
                                                arrowRange) (fun isMultiline ->
                onlyIf isMultiline sepNln
                +> sepCloseTFor rpr e.Range)
            |> genTriviaFor SynExpr_Paren pr
        ()
    )
