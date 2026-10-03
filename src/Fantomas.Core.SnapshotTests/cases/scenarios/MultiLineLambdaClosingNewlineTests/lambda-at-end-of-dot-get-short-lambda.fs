(*---
max_line_length = 50
fsharp_multi_line_lambda_closing_newline = true
---*)
configuration
    .MinimumLevel
    .Debug()
    .WriteTo
    .Logger(fun x -> x * x)
