(*---
fsharp_space_before_uppercase_invocation = true
fsharp_multi_line_lambda_closing_newline = true
---*)
Foobar(fun x ->
    // going multiline
    x * x)

myValue.UppercaseMemberCall(fun x ->
    let y = x + 1
    x + y)
