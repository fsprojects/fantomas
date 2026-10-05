(*---
max_line_length = 60
---*)
builder.Configure(function Some v -> handleSome v | None -> handleNone ()).Build().Result
