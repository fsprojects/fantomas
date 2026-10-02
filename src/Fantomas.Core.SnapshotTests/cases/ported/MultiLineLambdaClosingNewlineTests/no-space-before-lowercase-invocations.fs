(*---
fsharp_space_before_lowercase_invocation = false
fsharp_multi_line_lambda_closing_newline = true
---*)
foobar(fun x ->
    // going multiline
    x * x)

myValue.lowercaseMemberCall(fun x ->
    let y = x + 1
    x + y)
