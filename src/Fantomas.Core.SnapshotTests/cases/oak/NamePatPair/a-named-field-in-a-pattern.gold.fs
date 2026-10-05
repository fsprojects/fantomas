let area shape =
    match shape with
    | Rectangle(width = w; height = h) -> w * h
    | _ -> 0
