(*---
# A trivia comment after the lambda, before the closing parenthesis on its own line, gets no blank line before that parenthesis.
fsharp_multi_line_lambda_closing_newline = true
---*)
g
    x
    (fun () ->
        a()
        b()

    // A trailing comment.
    )
