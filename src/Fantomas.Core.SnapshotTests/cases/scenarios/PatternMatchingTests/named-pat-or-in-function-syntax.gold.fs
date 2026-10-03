let rec (|DoExprAttributesL|_|) =
    function
    | DoExpr _ | Attributes _ as x :: DoExprAttributesL(xs, ys) -> Some(x :: xs, ys)
    | DoExpr _ | Attributes _ as x :: ys -> Some([ x ], ys)
    | _ -> None
