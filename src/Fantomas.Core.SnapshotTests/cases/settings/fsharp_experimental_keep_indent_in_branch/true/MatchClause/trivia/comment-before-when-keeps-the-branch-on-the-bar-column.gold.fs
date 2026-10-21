let f rest =
    match Stream.peel rest with
    | None -> failwith "oh no"
    | Some longName
        // c
        when longName.IsEmpty |> not ->

    longName |> Map.map (fun _ -> TypedTerm.force<int>)
