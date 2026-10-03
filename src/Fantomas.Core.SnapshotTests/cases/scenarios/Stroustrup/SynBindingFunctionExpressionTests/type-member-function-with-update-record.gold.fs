type Foo() =
    member this.Bar x = {
        astContext with
            IsInsideMatchClausePattern = true
    }
