(*---
fsharp_multiline_bracket_style = cramped
---*)
[| Gen.map5 (fun b1 b2 expr1 expr2 pat ->
                            SynExpr.ForEach(DebugPointAtFor.No, SeqExprOnly b1, b2, pat, expr1, expr2, zero))
                            Arb.generate<_> Arb.generate<_> genSubDeclExpr genSubDeclExpr genSubSynPat |] //
