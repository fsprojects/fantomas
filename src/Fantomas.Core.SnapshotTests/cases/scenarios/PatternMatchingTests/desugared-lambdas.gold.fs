try
    fst (
        find
            (fun (s, (s', ty): int * int) ->
                s' = s0 && can (type_match ty ty0) [])
            (!the_interface)
    )
with Failure _ ->
    s0
