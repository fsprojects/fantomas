let f x =
    try
        foo ()
    with
    | A ->
        foo ()
        bar ()
        // comment
    | B -> ()
