things
|> Seq.map (fun a ->
    try
        Some i
    with
    | Foo _
    | Bar _ as e when true ->
        None
)
