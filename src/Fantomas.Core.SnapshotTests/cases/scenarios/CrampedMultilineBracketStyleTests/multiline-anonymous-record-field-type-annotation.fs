(*---
max_line_length = 80
fsharp_multiline_bracket_style = cramped
---*)
type ExprFolder<'State> =
    {| exprIntercept: ('State -> Expr -> 'State) -> ('State -> Expr -> 'State) -> 'State -> Exp -> 'State |}
