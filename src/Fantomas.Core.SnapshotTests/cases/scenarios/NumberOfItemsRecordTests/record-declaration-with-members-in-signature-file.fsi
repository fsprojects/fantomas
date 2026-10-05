(*---
fsharp_record_multiline_formatter = number_of_items
fsharp_newline_between_type_definition_and_members = false
fsharp_multiline_bracket_style = cramped
---*)
namespace X
type MyRecord =
    { Level: int
      Progress: string
      Bar: string
      Street: string
      Number: int }
    member Score : unit -> int
