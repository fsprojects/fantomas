let x =
    builder
        .Build()
        .Configure(
            function
            | Some v -> handleSome v
            | None -> handleNone ()
        )
