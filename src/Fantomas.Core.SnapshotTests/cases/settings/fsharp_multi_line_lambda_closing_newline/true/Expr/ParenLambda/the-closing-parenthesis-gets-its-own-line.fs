(*---
fsharp_multi_line_lambda_closing_newline = true
---*)
let doubled =
    List.map (fun x ->
        let y = x * 2
        y) numbers
