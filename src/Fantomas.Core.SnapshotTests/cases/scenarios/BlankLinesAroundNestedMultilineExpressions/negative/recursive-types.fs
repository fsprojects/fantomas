(*---
fsharp_blank_lines_around_nested_multiline_expressions = false
---*)
type Cmd<'msg> = Cmd'<'msg> list
and private Cmd'<'msg> = Send<'msg> -> unit
