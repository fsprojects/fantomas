let x =
    builder
        .Configure(function
            | Some v -> handleSome v
            | None -> handleNone ())
        .Build()
        .Result
