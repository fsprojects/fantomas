let shortExpr = genExpr e +> genSynLongIdent true sli
let longExpr = genExpr e +> indentSepNlnUnindent (genSynLongIdentMultiline true sli)

fun ctx ->
    isShortExpression
        ctx.Config.Blaaaaaaaaaaaaaaaaaaaaaaaaah
        shortExpr
        longExpr
        ctx
