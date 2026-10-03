let isText (value: obj) =
    match value with
    | :? string -> true
    | _ -> false
