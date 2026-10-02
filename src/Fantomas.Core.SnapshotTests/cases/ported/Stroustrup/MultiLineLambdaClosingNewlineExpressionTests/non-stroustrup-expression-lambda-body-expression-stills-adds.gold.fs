List.map (fun e ->
    try
        f e
    with ex ->
        "meh"
)
