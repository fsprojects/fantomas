(*---
fsharp_max_record_width = 10
fsharp_multiline_bracket_style = cramped
---*)
let r: {| Foo: int; Bar: string |} =
    {| Foo = 123
       Bar = "" |}
