type X() =
    member private this.Y with get() = "meh"
    member this.Z with private get() = "foo"
