type Foo() =
    member this.Bar x : MyRecord = {
        astContext with
            IsInsideMatchClausePattern = true
    }
