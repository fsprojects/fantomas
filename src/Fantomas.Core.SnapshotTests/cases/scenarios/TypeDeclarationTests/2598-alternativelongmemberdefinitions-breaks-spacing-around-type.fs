(*---
max_line_length = 80
fsharp_alternative_long_member_definitions = true
---*)
type Server<'a>
    (
        clusterSize : int,
        persistentState : IPersistentState<'a>,
        messageChannel : int<ServerId> -> Message<'a> -> unit
    ) as this =
    let mutable i = 0
    member this.Blah = 0
