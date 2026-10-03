let f x =
    match x with
    | 1 ->
        foo ()
        bar ()
        // comment
    | _ -> ()
    |> ignore
