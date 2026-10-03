(*---
fsharp_max_record_width = 10
fsharp_multiline_bracket_style = cramped
---*)
let r: struct {| Foo: int; Bar: string |} =
    struct {| Foo = 123
              Bar = "" |}
