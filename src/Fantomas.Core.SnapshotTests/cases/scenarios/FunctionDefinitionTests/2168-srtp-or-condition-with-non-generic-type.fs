let inline deserialize< ^a when ( ^a or FromJsonDefaults) : (static member FromJson :  ^a -> Json< ^a>)> json =
    json |> Json.parse |> Json.deserialize< ^a>
