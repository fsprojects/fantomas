type Foo() =
    member this.Item
        with get (name: string): obj option = None

    member this.Item
        with set (name: string) (v: obj option): unit = ()
