type Foo() =
    member this.Bar x : {| A: int; B: int; C: int |} = struct {|
        A = longTypeName
        B = someOtherVariable
        C = ziggyBarX
    |}
