/// A set of function parameters (visitor) for folding over expressions
type ExprFolder<'State> =
    { exprIntercept:
        ('State -> Expr -> 'State) (* noInterceptF *)
            -> ('State -> Expr -> 'State)
            -> 'State
            -> Expr
            -> 'State (* recurseF *)
      valBindingSiteIntercept: 'State -> bool * Val -> 'State
      nonRecBindingsIntercept: 'State -> Binding -> 'State
      recBindingsIntercept: 'State -> Bindings -> 'State
      dtreeIntercept: 'State -> DecisionTree -> 'State
      targetIntercept: ('State -> Expr -> 'State) -> 'State -> DecisionTreeTarget -> 'State option
      tmethodIntercept: ('State -> Expr -> 'State) -> 'State -> ObjExprMethod -> 'State option }
