(*---
fsharp_space_before_colon = true
fsharp_space_before_semicolon = true
fsharp_newline_between_type_definition_and_members = false
---*)
type X = {
    Y : int
} with // foo
    member x.Z = ()
