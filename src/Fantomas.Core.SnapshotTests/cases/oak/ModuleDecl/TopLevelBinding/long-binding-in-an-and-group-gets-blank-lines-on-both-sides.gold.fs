let rec f x = g x

and g x =
    match x with
    | 0 -> true
    | _ -> h (x - 1)

and h x = f x
