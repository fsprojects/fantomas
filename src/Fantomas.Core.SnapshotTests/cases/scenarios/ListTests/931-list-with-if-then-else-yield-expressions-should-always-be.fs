(*---
fsharp_max_if_then_short_width = 120
fsharp_max_if_then_else_short_width = 120
fsharp_max_array_or_list_width = 120
fsharp_multiline_bracket_style = cramped
---*)
let original_input = [
  if true then yield "value1"
  if false then yield "value2"
  if true then yield "value3"
]
