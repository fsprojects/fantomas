let foo () =
    f ()
    |> fun x -> if x then 1 else 2
    |> g
