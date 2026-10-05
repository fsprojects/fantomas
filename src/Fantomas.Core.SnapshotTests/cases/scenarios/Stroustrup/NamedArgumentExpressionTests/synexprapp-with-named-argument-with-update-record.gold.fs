let v =
    SomeConstructor(
        v = {
            astContext with
                IsInsideMatchClausePattern = true
                A = longTypeName
                B = someOtherVariable
                C = ziggyBarX
        }
    )
