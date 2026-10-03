let a ex =
    if null = ex then
        fooo ()
        None
        // this was None
    elif ex.GetType() = typeof<obj> then
        Some ex
    else
        None
