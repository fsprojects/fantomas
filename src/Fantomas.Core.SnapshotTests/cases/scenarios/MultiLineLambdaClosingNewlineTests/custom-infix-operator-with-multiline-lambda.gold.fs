let expr =
    genExpr astContext e
    +> col
        sepSpace
        es
        (fun e ->
            match e with
            | Paren(_, Lambda _, _) -> !-"lambda"
            | _ -> genExpr astContext e
        )
