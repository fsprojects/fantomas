let validate input =
    if String.IsNullOrWhiteSpace input then
        Error "empty"
    else

    let trimmed = input.Trim()
    Ok trimmed
