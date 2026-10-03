type Foo() =
    member this.Bar x : {| A: int; B: int; C: int |} = {|
        A = longTypeName
        B = someOtherVariable
        C = ziggyBarX
    |}
