type SingleAppParenLambda
    (
        // Expr could be a single identifier or TypeApp
        functionName: Expr, parenLambda: ExprParenLambdaNode, range
    ) =
    inherit NodeBase(range)
    override this.Children = [| yield Expr.Node functionName; yield parenLambda |]
    member x.FunctionName = functionName
    member x.ParenLambda = parenLambda
