type Server<'a>
    (
        clusterSize: int,
        persistentState: IPersistentState<'a>,
        messageChannel: int<ServerId> -> Message<'a> -> unit
    )
    as this =
    let mutable i = 0
    member this.Blah = 0
