(*---
fsharp_max_value_binding_width = 120
fsharp_newline_between_type_definition_and_members = false
---*)
open System
type Exception with
    member inline __.FirstLine =
        __.Message.Split([|Environment.NewLine|], StringSplitOptions.RemoveEmptyEntries).[0]
