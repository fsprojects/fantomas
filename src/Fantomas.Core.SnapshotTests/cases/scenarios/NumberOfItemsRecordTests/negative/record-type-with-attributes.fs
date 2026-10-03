(*---
fsharp_record_multiline_formatter = number_of_items
fsharp_multiline_bracket_style = cramped
---*)
[<Foo>]
type Args =
    { [<Foo "">]
      [<Bar>]
      [<Baz 1>]
      Hi: int list }

module Foo =

    let r = 3
