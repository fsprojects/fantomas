let prefetchImages =
    [ playerOImage; playerXImage ]
    |> List.map (fun img -> link [ Rel "prefetch"; Href img ])