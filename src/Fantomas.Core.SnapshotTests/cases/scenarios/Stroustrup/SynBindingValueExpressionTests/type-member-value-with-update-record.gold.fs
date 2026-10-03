type Foo() =
    member this.Bar = {
        astContext with
            IsInsideMatchClausePattern = true
    }
