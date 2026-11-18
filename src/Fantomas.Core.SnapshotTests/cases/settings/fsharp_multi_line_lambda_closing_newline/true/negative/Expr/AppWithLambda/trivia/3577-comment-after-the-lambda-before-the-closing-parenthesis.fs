(*---
# The narrow line makes `false` break the call over several lines too, so neither value changes it.
fsharp_multi_line_lambda_closing_newline = true
max_line_length = 20
---*)
someFunction
    x
    (fun () ->
        a ()
        b ()

    // A trailing comment.
    )
