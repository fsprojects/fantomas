if
    List.exists
        (function
        | CompExpr _ -> true
        | _ -> false)
        es
then
    shortExpression ctx
else
    expressionFitsOnRestOfLine shortExpression longExpression ctx
