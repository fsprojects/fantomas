opt {
    let! abc = def ()

    and! foo = {
        new IFoo with
            member _.Bar() = longTypeName
            member _.Baz() = someOtherVariable
    }

    ()
}
