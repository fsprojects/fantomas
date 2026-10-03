let foo =
    [ 1 ]
    |> List.sort
    #if DEBUG
    |> List.rev
    #endif
    |> List.sort
