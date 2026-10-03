things
|> Seq.map (fun a ->
    try
        Some i
    with
    | :? Foo
    | :? Bar as e when true -> None)
