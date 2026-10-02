(*---
fsharp_space_before_colon = true
fsharp_space_before_semicolon = true
---*)
[<Foo>]
type Args =
    { [<Foo "">]
      [<Bar>]
      [<Baz 1>]
      Hi: int list }

module Foo =

    let r = 3
