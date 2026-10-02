let x =
    if
        try
            true
        with Failure _ ->
            false
    then
        ()
    else
        ()
