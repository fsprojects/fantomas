type Foo = Foo of string

and Bar =
    interface IMeh with
        [<SomeAttribute>]
        member this.Value = ()

        [<SomeAttribute>]
        member this.ValueWithReturnType: unit = ()

        [<SomeAttribute>]
        member this.SomeFunction(a: int) = 4 + a

        [<SomeAttribute>]
        member this.SomeFunctionWithReturnType(a: int) : int = a + 5
