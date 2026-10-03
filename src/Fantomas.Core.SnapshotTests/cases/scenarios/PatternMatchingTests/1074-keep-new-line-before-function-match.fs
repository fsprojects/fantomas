    let (|AndExpr|_|) =
        let chooser =
            function
            | (ExprPat e1, ExprPat e2) -> Some(e1, e2)
            | _ -> None

        function
        | ListSplitPick "&&" chooser (e1, e2) -> Some(BoolExpr.And(e1, e2))
        | _ -> None
