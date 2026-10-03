let value =
    [
        if foo then yield! [ "a"; "b" ] else yield "c"
        if bar then yield "d" else yield! [ "e"; "f" ]
    ]
