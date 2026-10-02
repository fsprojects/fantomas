let a =
    Something.foo
        bar
        meh
        (function
        | Ok x -> true
        | Error err -> false
        )
