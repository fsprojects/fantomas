(*---
fsharp_multi_line_lambda_closing_newline = true
---*)
module Foo =

    let bar =
        baz
        |> (
            // Hi!
            match false with
            | true -> id
            | false -> id
        )
