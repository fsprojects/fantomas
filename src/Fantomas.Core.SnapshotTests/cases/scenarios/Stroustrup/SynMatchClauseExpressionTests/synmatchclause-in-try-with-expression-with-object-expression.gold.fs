try
    foo ()
with ex -> {
    new IFoo with
        member _.Bar() = longTypeName
        member _.Baz() = someOtherVariable
}
