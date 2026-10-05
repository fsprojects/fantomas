let f x =
    try
        foo ()
        bar ()
        // comment
    finally
        baz ()
