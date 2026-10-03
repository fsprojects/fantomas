(*---
fsharp_space_before_uppercase_invocation = true
---*)
Log.Logger <-
    LoggerConfiguration<Foo>()
        .Destructure.FSharpTypes()
        .WriteTo.Console()
        .CreateLogger()
