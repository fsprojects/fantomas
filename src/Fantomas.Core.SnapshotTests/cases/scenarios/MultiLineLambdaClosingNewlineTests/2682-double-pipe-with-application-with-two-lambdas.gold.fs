(someLongItemOne, someLongItemTwo)
||> Prefix.fnName
        (fun delta echo -> delta, echo)
        (fun (k: One * Two * Three) ->
            // multiline
            ()
        )
        lastArgument

(someLongItemOne, someLongItemTwo)
|> Prefix.fnName
    (fun delta echo -> delta, echo)
    (fun (k: One * Two * Three) ->
        // multiline
        ()
    )
    lastArgument
