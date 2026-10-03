let result =
    try
        work ()
    with
    | :? System.TimeoutException -> retry ()
    | ex -> fallback ex
