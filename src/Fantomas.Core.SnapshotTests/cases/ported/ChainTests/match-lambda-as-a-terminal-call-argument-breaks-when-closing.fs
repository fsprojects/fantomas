(*---
fsharp_multi_line_lambda_closing_newline = true
---*)
let x =
    builder.Build().Configure(function
        | Some v -> handleSome v
        | None -> handleNone ())
