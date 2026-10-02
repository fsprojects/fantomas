(*---
fsharp_multi_line_lambda_closing_newline = true
---*)
module Foo =
    let bar =
        []
        |> List.choose (
            function
            | _ -> ""
        )
