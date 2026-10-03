module Telplin

type T =
    struct
        member private this.X
            with get (): int = 1
            and set (_: int) = ()
    end
