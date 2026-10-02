type C () =
    let rec g x = h x
    and h x = g x

    member x.P = g 3