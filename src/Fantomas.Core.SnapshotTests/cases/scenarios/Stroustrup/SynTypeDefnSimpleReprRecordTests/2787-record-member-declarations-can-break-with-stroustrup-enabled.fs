(*---
fsharp_newline_between_type_definition_and_members = false
fsharp_multiline_bracket_style = stroustrup
---*)
type SomeEvent =
    { Id: string
      Name: string }
    member x.BreakWithOtherStuffAs well = ()

type UpdatedName = { PreviousName: string }
