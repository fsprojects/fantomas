(*---
fsharp_newline_between_type_definition_and_members = false
fsharp_multiline_bracket_style = stroustrup
---*)
type X = {
    Y: int
} with // foo
    member x.Z = ()
