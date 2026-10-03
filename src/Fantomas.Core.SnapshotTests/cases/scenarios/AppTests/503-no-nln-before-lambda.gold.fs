let a =
    b
    |> List.exists (fun p ->
        p.a
        && p.b
           |> List.exists (fun o -> o.a = "lorem ipsum dolor sit amet"))
