type ExprFolder<'State> =
    {| exprIntercept:
        ('State -> Expr -> 'State)
            -> ('State -> Expr -> 'State)
            -> 'State
            -> Exp
            -> 'State |}
