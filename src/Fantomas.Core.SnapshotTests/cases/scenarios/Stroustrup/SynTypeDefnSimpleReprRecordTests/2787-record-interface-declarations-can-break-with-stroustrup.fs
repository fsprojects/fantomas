(*---
fsharp_newline_between_type_definition_and_members = false
fsharp_multiline_bracket_style = stroustrup
---*)
type IEvent = interface end

type SomeEvent =
    { Id: string
      Name: string }
    interface IEvent

type UpdatedName = { PreviousName: string }
