(*---
max_line_length = 60
---*)
builder.Configure(fun v -> handleSomeValue v |> andThenSomethingElse v).Build().Result
