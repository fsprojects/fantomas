match synExpr with
| SynExpr.App(
    argExpr = SynExpr.Match _ // CCC
    ) -> Some ident.idRange
| _ -> defaultTraverse synExpr
