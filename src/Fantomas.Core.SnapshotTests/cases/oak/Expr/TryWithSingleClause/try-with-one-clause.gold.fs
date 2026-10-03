let result =
    try
        work ()
    with ex ->
        fallback ex
