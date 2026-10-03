(*---
indent_size = 2
fsharp_space_before_colon = true
---*)
let readModel (updateState : 'State -> EventEnvelope<'Event> list -> 'State) (initState : 'State) : ReadModel<'Event, 'State> =
    ()
