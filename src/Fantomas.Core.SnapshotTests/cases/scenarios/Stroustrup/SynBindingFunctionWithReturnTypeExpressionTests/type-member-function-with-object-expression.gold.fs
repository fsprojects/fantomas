type Foo() =
    member this.Bar x : IFoo = {
        new IFoo with
            member _.Bar() = longTypeName
            member _.Baz() = someOtherVariable
    }
