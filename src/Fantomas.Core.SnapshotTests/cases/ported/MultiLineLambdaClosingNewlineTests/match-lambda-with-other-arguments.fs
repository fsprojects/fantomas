(*---
fsharp_multi_line_lambda_closing_newline = true
---*)
let a =
    Something.foo
        bar
        meh
        (function | Ok x -> true | Error err -> false)
