let first =
    (line.Split(
        [| ":" |],
        StringSplitOptions.RemoveEmptyEntries
    ))
        .Length
