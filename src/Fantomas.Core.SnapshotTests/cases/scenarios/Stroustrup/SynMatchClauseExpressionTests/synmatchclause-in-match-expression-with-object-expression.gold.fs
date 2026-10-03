match x with
| _ -> {
    new IFoo with
        member _.Bar() = longTypeName
        member _.Baz() = someOtherVariable
  }
