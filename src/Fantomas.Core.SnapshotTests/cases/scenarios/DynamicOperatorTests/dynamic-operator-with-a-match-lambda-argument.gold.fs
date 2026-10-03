let b =
    x?y (function
        | Some v -> v
        | None -> 0)
