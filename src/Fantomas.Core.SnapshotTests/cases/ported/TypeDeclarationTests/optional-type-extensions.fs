(*---
fsharp_max_function_binding_width = 120
fsharp_newline_between_type_definition_and_members = false
---*)
/// Define a new member method FromString on the type Int32.
type System.Int32 with
    member this.FromString( s : string ) =
       System.Int32.Parse(s)