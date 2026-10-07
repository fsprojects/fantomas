(*---
fsharp_multiline_bracket_style = stroustrup
fsharp_max_record_width = 0
max_line_length = 20
---*)
f<{| a: int |},
#if DEBUG
 int -> int -> int -> string
#else
 {| b: bool |} list
#endif
 >
