opt {
    let! foo = {
        new IFoo with
            member _.Bar() = longTypeName
            member _.Baz() = someOtherVariable
    }

    ()
}
