let inline deserialize< ^a when ^a: (static member FromJson: ^a -> Json< ^a >)> json =
    json |> Json.parse |> Json.deserialize< ^a>
