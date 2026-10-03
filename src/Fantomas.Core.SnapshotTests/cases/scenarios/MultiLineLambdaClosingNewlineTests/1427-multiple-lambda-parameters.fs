(*---
fsharp_space_before_lowercase_invocation = false
fsharp_multi_line_lambda_closing_newline = true
---*)
let choose chooser source =
    source
    |> Set.fold
        (fun set item ->
            chooser item
            |> Option.map (fun mappedItem -> Set.add mappedItem set)
            |> Option.defaultValue set)
        Set.empty
