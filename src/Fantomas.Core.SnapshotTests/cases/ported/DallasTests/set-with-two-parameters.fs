type X =
    member this.Item
            with get (name: string): obj option = None
            and set (name: string) (v: obj option): unit = ()
