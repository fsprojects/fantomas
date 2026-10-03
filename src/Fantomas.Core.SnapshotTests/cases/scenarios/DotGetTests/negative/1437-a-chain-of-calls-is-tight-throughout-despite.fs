(*---
max_line_length = 40
fsharp_space_before_uppercase_invocation = true
---*)
Log.Logger <-
    LoggerConfiguration()
        .Destructure.FSharpTypes()
        .WriteTo.Console()
        .CreateLogger()
