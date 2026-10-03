(*---
indent_size = 2
fsharp_multiline_bracket_style = cramped
---*)
   let tryDataOperation  =

           let body =
             let clauses =
               [ mkSynMatchClause
                   (mkSynPatLongIdentSimple "Some")
                   (mkSynExprAppNonAtomic
                     (mkSynExprLongIdent $"this.{memberName}")
                     (mkSynExprParen (
                       mkSynExprTuple
                         [ mkSynExprIdent "state" ]
                     ))) ]

             mkSynExprMatch clauses

           mkMember $"this.Try{memberName}" None [ mkSynAttribute "CustomOperation" (mkSynExprConstString $"try{memberName}") ] [ parameters ] (objectStateExpr body)
