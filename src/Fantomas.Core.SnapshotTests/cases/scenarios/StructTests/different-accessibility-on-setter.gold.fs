module Telplin

type T =
    struct
        member this.X
            with get (): int = 1
            and private set (_: int) = ()

        member this.Y
            with internal get (): int = 1
            and private set (_: int) = ()

        member private this.Z
            with get (): int = 1
            and set (_: int) = ()

        member this.S
            with internal set (_: int) = ()
            and private get (): int = 1
    end
