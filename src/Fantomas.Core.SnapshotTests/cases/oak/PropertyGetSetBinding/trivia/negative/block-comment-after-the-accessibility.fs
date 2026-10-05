type T() =
    member this.X
        with private (* c *) get (i: int) = i
