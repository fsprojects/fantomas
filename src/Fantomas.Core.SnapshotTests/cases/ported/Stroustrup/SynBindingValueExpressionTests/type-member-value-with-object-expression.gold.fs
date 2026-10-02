type Foo() =
    member this.Bar = {
        new IFoo with
            member _.Bar() = longTypeName
            member _.Baz() = someOtherVariable
    }
