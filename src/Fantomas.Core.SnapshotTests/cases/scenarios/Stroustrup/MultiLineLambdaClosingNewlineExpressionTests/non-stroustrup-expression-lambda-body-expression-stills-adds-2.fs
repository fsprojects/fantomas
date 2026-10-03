(*---
fsharp_max_array_or_list_width = 40
fsharp_multi_line_lambda_closing_newline = true
fsharp_multiline_bracket_style = stroustrup
---*)
fn a b c (fun e ->
    try
        f e
    with
    | ex -> "meh")
