(*---
fsharp_multi_line_lambda_closing_newline = true
---*)
module Foo =
    let blah =
        printfn ""
        (fun bar ->
            printfn ""
            bar + "11111111111111111111111111111111"
        )
