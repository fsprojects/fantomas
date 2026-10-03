let fooDto = {|
    otherDto with
        TextFilters =
            criteria.Meta.TextFilter
            |> Option.map (fun f -> f.Filters)
            |> Option.map (List.map (sprintf "~%s~"))
            |> Option.toObj
|}
