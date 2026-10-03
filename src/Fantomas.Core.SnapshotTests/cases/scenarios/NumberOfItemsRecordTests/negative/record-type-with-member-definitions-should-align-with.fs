(*---
fsharp_record_multiline_formatter = number_of_items
fsharp_max_value_binding_width = 120
fsharp_newline_between_type_definition_and_members = false
fsharp_multiline_bracket_style = cramped
---*)
type Range =
    { From: float
      To: float }
    member this.Length = this.To - this.From
