fn
    a
    b
    c
    (fun e ->
        try
            f e
        with ex ->
            "meh"
    )
