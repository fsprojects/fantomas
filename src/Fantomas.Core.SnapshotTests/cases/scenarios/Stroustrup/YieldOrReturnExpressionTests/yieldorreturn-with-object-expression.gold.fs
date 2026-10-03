myComp {
    yield {
        new IFoo with
            member _.Bar() = longTypeName
            member _.Baz() = someOtherVariable
    }

    return {
        new IFoo with
            member _.Bar() = longTypeName
            member _.Baz() = someOtherVariable
    }
}
