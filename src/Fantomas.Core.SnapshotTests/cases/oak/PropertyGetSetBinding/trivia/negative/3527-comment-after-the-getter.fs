type T() =
    member _.X
        with get () = 1 // get
        and set (_: int) = ()
