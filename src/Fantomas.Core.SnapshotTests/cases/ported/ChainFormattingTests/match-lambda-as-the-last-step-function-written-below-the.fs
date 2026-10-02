(*---
max_line_length = 60
---*)
builder.Build().Configure(
    function
    | Some v -> handleSome v
    | None -> handleNone ())
