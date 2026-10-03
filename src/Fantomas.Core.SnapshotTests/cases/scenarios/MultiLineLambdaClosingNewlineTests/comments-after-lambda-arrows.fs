(*---
fsharp_multi_line_lambda_closing_newline = true
---*)
[]
|> List.map (fun foo -> // I use the name foo a lot
    foo + 1)

List.map(fun bar -> // same remark for bar
    bar + 2) []
