let expected =
    b
    |> function
        | Some c -> c
        | None -> 0
    |> id
