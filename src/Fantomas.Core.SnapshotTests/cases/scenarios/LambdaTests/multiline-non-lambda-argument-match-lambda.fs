leadingExpressionIsMultiline (sepOpenTFor lpr -- "fun "
                                +> pats
                                +> genArrowWithTrivia
                                    (genExprKeepIndentInBranch astContext bodyExpr)
                                    arrowRange) (function | Ok _ -> true | Error _ -> false)
