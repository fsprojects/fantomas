let rec f x =
    match x with
    | 0 -> false
    | _ -> g (x - 1) // after f

and (* before g *) g x =
    match x with
    | 0 -> true
    | _ -> f (x - 1)
