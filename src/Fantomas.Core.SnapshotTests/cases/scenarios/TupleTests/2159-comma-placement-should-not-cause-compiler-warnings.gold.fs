let f x =
    React.useEffect (
        fun () ->
            if length x > 5 && length x < 10 then
                doX x
            else
                doY x
        , [| x |]
    )

    ()
