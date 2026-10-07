(*---
fsharp_multiline_bracket_style = stroustrup
fsharp_max_record_width = 0
---*)
type T = Outer< @"foo", Inner<{| a: int |}>>
