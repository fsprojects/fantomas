(*---
indent_size = 2
fsharp_space_before_colon = true
fsharp_align_function_signature_to_indentation = true
---*)
let readModel (updateState : 'State -> EventEnvelope<'Event> list -> 'State) (initState : 'State) : ReadModel<'Event, 'State> =
    ()
