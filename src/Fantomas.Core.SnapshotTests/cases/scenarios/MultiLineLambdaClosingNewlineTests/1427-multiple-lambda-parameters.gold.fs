let choose chooser source =
    source
    |> Set.fold
        (fun set item ->
            chooser item
            |> Option.map(fun mappedItem -> Set.add mappedItem set)
            |> Option.defaultValue set
        )
        Set.empty
