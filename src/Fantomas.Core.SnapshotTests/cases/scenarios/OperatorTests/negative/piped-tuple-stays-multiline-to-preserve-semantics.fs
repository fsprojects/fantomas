fun i -> sprintf "%i" i, fun () -> i
|> List.init foo
|> Map.ofList
