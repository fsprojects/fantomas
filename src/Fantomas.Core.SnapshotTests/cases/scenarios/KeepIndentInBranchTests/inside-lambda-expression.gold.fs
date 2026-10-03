let foo =
    bar
    |> List.filter (fun i ->
        if false then
            false
        else

        let m = quux
        quux.Success && somethingElse)
