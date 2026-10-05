type Y =
    member this.X
        with get (): int = 1
        and private set (_: int) = ()
