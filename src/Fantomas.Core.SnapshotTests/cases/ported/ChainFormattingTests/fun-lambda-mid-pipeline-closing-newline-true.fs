(*---
max_line_length = 60
fsharp_multi_line_lambda_closing_newline = true
---*)
builder.Configure(fun v -> handleSomeValue v |> andThenSomethingElse v).Build().Result
