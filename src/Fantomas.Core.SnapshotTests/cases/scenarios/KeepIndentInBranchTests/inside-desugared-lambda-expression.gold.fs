let foo =
    bar
    |> List.filter (fun { Index = i } ->
        if false then
            false
        else

        let m = quux
        quux.Success && somethingElse)
