type SomeType() =

    let mutable v : string = ""

    member val SomeAutoProp : float = 23.42 with get, set

    member this.MyProperty
        with get () : string = v
        and set (value : string) : unit = v <- value
