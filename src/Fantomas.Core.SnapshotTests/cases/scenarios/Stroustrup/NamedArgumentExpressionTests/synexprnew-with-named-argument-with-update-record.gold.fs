let v =
    new FooBar(
        v = {
            astContext with
                IsInsideMatchClausePattern = true
                A = longTypeName
                B = someOtherVariable
                C = ziggyBarX
        }
    )
