let v =
    new FooBar(
        v = {
            new IFoo with
                member _.Bar() = longTypeName
                member _.Baz() = someOtherVariable
        }
    )
