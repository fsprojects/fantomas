(*---
fsharp_multi_line_lambda_closing_newline = true
---*)
module Foo =
    let blah =
        printfn ""
        // meh
        (fun bar ->  // foo
            printfn ""
            bar + "11111111111111111111111111111111"
            // bar
        ) // ziggy
